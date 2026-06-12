"""
Baseball Savant Minor League Scraper - FINAL VERSION
Downloads CSV files for each pitcher by clicking Graphs -> Download as CSV
"""

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import os
import glob

def scrape_pitchers(year, num_pitchers=None, output_dir=None):
    """
    Scrape pitcher CSV files from Baseball Savant
    
    Args:
        year: Season year (e.g., 2025)
        num_pitchers: Number of pitchers to scrape (None = all)
        output_dir: Where to save files (default: ~/Downloads/baseball_savant_data)
    """
    # Setup download directory
    if output_dir is None:
        output_dir = os.path.expanduser("~/Downloads/baseball_savant_data")
    os.makedirs(output_dir, exist_ok=True)
    
    print(f"\n{'='*60}")
    print(f"Baseball Savant Scraper - {year}")
    print(f"{'='*60}")
    print(f"Output directory: {output_dir}")
    
    # Setup Chrome with download preferences
    chrome_options = Options()
    chrome_options.binary_location = "/Applications/Google Chrome 2.app/Contents/MacOS/Google Chrome"
    
    prefs = {
        "download.default_directory": output_dir,
        "download.prompt_for_download": False,
        "download.directory_upgrade": True,
        "safebrowsing.enabled": True
    }
    chrome_options.add_experimental_option("prefs", prefs)
    
    chromedriver_path = os.path.expanduser("~/Downloads/chromedriver")
    service = Service(chromedriver_path)
    
    try:
        print("\nOpening browser...")
        driver = webdriver.Chrome(service=service, options=chrome_options)
        
        # Navigate to search results with min_pitches=100 filter
        url = (
            f"https://baseballsavant.mlb.com/statcast-search-minors?"
            f"hfPT=&hfAB=&hfGT=R%7C&hfPR=&hfZ=&hfStadium=&hfBBL=&hfNewZones=&"
            f"hfPull=&hfC=&hfSea={year}%7C&hfSit=&player_type=pitcher&hfOuts=&"
            f"home_road=&pitcher_throws=&batter_stands=&hfSA=&hfEventOuts=&"
            f"hfEventRuns=&game_date_gt=&game_date_lt=&hfMo=&hfTeam=&hfOpponent=&"
            f"hfRO=&position=&hfInn=&hfBBT=&hfFlag=is%5C.%5C.tracked%7C&hfLevel=&"
            f"metric_1=&hfTeamAffiliate=&hfOpponentAffiliate=&"
            f"group_by=name&min_pitches=100&min_results=0&min_pas=0&"
            f"sort_col=pitches&player_event_sort=api_p_release_speed&"
            f"sort_order=desc&chk_is..tracked=on#results"
        )
        
        print(f"Loading search results for {year}...")
        driver.get(url)
        time.sleep(5)
        
        # Find the results table
        print("Finding pitcher table...")
        table = driver.find_element(By.TAG_NAME, "table")
        rows = table.find_elements(By.TAG_NAME, "tr")
        
        # Skip header and count only data rows (not separator rows)
        data_rows = rows[1:]
        
        # Count actual pitcher rows (rows with more than 1 cell)
        actual_pitcher_rows = []
        for row in data_rows:
            cells = row.find_elements(By.TAG_NAME, "td")
            if len(cells) > 1:  # Skip separator rows (1 cell)
                actual_pitcher_rows.append(row)
        
        total_pitchers = len(actual_pitcher_rows)
        
        print(f"✓ Found {total_pitchers} pitchers with data")
        
        if num_pitchers is None:
            num_pitchers = total_pitchers
        else:
            num_pitchers = min(num_pitchers, total_pitchers)
        
        print(f"\nScraping {num_pitchers} pitchers...")
        print(f"{'='*60}\n")
        
        # Check for existing files to skip
        existing_files = set()
        if os.path.exists(output_path):
            existing_files = {f for f in os.listdir(output_path) if f.endswith('.csv')}
            if existing_files:
                print(f"Found {len(existing_files)} existing files")
                skip = input("Skip already downloaded pitchers? (y/n): ").strip().lower()
                if skip != 'y':
                    existing_files = set()
                else:
                    print(f"Will skip {len(existing_files)} already downloaded pitchers\n")
        
        start_time = time.time()
        successful = 0
        failed = 0
        skipped = 0
        
        pitcher_index = 0  # Track actual pitcher rows
        
        print(f"\nProgress: [0/{num_pitchers}] 0% complete")
        
        for i in range(num_pitchers):
            # Keep trying rows until we process num_pitchers actual pitchers
            while pitcher_index < len(data_rows):
                # Show progress every 10 pitchers
                if successful > 0 and successful % 10 == 0:
                    percent = int((successful / num_pitchers) * 100)
                    elapsed_time = time.time() - start_time
                    avg_time = elapsed_time / successful
                    remaining = (num_pitchers - successful) * avg_time
                    eta_minutes = int(remaining / 60)
                    print(f"\n{'='*60}")
                    print(f"Progress: [{successful}/{num_pitchers}] {percent}% complete")
                    print(f"Elapsed: {int(elapsed_time/60)} min | ETA: ~{eta_minutes} min remaining")
                    print(f"{'='*60}\n")
                
                print(f"[{successful + failed + 1}/{num_pitchers}] Processing pitcher...")
                
                try:
                    # Reload the results page fresh each time
                    driver.get(url)
                    time.sleep(5)
                    
                    # Scroll to top
                    driver.execute_script("window.scrollTo(0, 0);")
                    time.sleep(1)
                    
                    # Find table and rows again
                    table = driver.find_element(By.TAG_NAME, "table")
                    rows = table.find_elements(By.TAG_NAME, "tr")
                    data_rows = rows[1:]  # Skip header
                    
                    if pitcher_index >= len(data_rows):
                        print("  ⚠️  Row not found")
                        break
                    
                    target_row = data_rows[pitcher_index]
                    
                    # Scroll row into view and wait for it to be ready
                    driver.execute_script("arguments[0].scrollIntoView({block: 'center'});", target_row)
                    time.sleep(3)
                    
                    # Get all cells in this row
                    cells = target_row.find_elements(By.TAG_NAME, "td")
                    
                    # Check if this is a valid data row
                    if len(cells) <= 1:
                        # This is a separator row, skip it
                        pitcher_index += 1
                        skipped += 1
                        continue
                    
                    # Get pitcher name
                    if len(cells) <= 2:
                        pitcher_index += 1
                        skipped += 1
                        continue
                    
                    pitcher_name_cell = cells[2]
                    pitcher_name = pitcher_name_cell.text.strip()
                    
                    if not pitcher_name:
                        pitcher_index += 1
                        skipped += 1
                        continue
                    
                    print(f"  Pitcher: {pitcher_name}")
                    
                    # Check if already downloaded
                    if existing_files:
                        # Check if any file starts with this pitcher's name pattern
                        pitcher_pattern = pitcher_name.replace(', ', '_').replace(' ', '_')[:20]  # First 20 chars
                        already_downloaded = any(pitcher_pattern in f for f in existing_files)
                        if already_downloaded:
                            print(f"  ⊝ Already downloaded (skipping)")
                            pitcher_index += 1
                            skipped += 1
                            continue
                    
                    # Find graphs div with retries
                    graphs_div = None
                    max_attempts = 3
                    
                    for attempt in range(max_attempts):
                        if attempt > 0:
                            print(f"    Retry {attempt}/{max_attempts-1}...")
                            time.sleep(2)
                        
                        for cell in cells:
                            try:
                                potential_div = cell.find_element(By.CLASS_NAME, "graphs")
                                if potential_div.text.strip() == "Graphs" and potential_div.is_displayed():
                                    graphs_div = potential_div
                                    break
                            except:
                                continue
                        
                        if graphs_div:
                            break
                    
                    if not graphs_div:
                        print(f"  ⚠️  Could not find Graphs button after {max_attempts} attempts")
                        failed += 1
                        pitcher_index += 1
                        continue
                    
                    # Count files before download
                    files_before = set(glob.glob(os.path.join(output_dir, "*.csv")))
                    
                    # Click Graphs to open popup menu
                    print("  Clicking Graphs...")
                    driver.execute_script("arguments[0].click();", graphs_div)
                    time.sleep(4)
                    
                    # Find the popup that appeared (chart-options)
                    print("  Finding popup menu...")
                    popup = None
                    try:
                        popup = driver.find_element(By.CLASS_NAME, "chart-options")
                        if not popup.is_displayed():
                            popup = None
                    except:
                        popup = None
                    
                    if not popup:
                        print("  ⚠️  Popup menu did not appear")
                        failed += 1
                        pitcher_index += 1
                        continue
                    
                    # Find CSV download div INSIDE the popup
                    print("  Clicking Download CSV (inside popup)...")
                    try:
                        csv_div = popup.find_element(By.CLASS_NAME, "csv")
                        driver.execute_script("arguments[0].click();", csv_div)
                    except Exception as e:
                        print(f"  ⚠️  Could not find CSV button in popup: {e}")
                        failed += 1
                        pitcher_index += 1
                        continue
                    
                    # Wait for new file to appear
                    print("  Waiting for download...")
                    new_file = None
                    for attempt in range(10):  # Try for 10 seconds
                        time.sleep(1)
                        files_after = set(glob.glob(os.path.join(output_dir, "*.csv")))
                        new_files = files_after - files_before
                        if new_files:
                            new_file = list(new_files)[0]
                            break
                    
                    if not new_file:
                        print("  ⚠️  Download did not complete")
                        failed += 1
                        pitcher_index += 1
                        continue
                    
                    # Rename the file
                    clean_name = pitcher_name.replace(", ", "_").replace(" ", "_").replace("/", "_").replace("#", "")
                    new_filename = f"{clean_name}_{year}.csv"
                    new_filepath = os.path.join(output_dir, new_filename)
                    
                    # Handle duplicates
                    if os.path.exists(new_filepath):
                        counter = 2
                        while os.path.exists(os.path.join(output_dir, f"{clean_name}_{year}_{counter}.csv")):
                            counter += 1
                        new_filepath = os.path.join(output_dir, f"{clean_name}_{year}_{counter}.csv")
                    
                    os.rename(new_file, new_filepath)
                    print(f"  ✓ Saved as: {os.path.basename(new_filepath)}")
                    
                    successful += 1
                    pitcher_index += 1
                    break  # Successfully processed this pitcher, move to next
                    
                except Exception as e:
                    print(f"  ❌ Error: {e}")
                    
                    # Check if it's a session error (browser crashed)
                    if 'invalid session id' in str(e).lower() or 'session' in str(e).lower():
                        print("  🔄 Browser session lost - restarting browser...")
                        try:
                            driver.quit()
                        except:
                            pass
                        
                        # Restart browser
                        try:
                            service = Service(chromedriver_path)
                            driver = webdriver.Chrome(service=service, options=chrome_options)
                            print("  ✓ Browser restarted")
                        except Exception as restart_error:
                            print(f"  ❌ Could not restart browser: {restart_error}")
                            failed += 1
                            pitcher_index += 1
                            break
                    
                    failed += 1
                    pitcher_index += 1
                    break  # Skip this pitcher and move to next
        
        driver.quit()
        
        total_time = time.time() - start_time
        
        # Summary
        print(f"\n{'='*60}")
        print("SCRAPING COMPLETE")
        print(f"{'='*60}")
        print(f"✓ Successful: {successful}")
        print(f"✗ Failed: {failed}")
        if skipped > 0:
            print(f"⊝ Skipped separator rows: {skipped}")
        print(f"⏱  Total time: {int(total_time/60)} minutes {int(total_time%60)} seconds")
        if successful > 0:
            print(f"⏱  Average: {total_time/successful:.1f} seconds per pitcher")
        
        # Check downloads
        csv_files = glob.glob(os.path.join(output_dir, "*.csv"))
        print(f"\n📁 Files in output directory: {len(csv_files)}")
        
        if csv_files:
            total_size_mb = sum(os.path.getsize(f) for f in csv_files) / (1024*1024)
            print(f"📊 Total size: {total_size_mb:.1f} MB")
            print(f"\n✅ All files saved to:\n   {output_dir}")
        
        return successful, failed
        
    except Exception as e:
        print(f"\n❌ Fatal error: {e}")
        import traceback
        traceback.print_exc()
        return 0, 0


def main():
    print("⚾ Baseball Savant Minor League Scraper")
    print("="*60)
    
    year = input("Which year? (e.g., 2025): ").strip()
    if not year:
        print("No year entered")
        return
    
    num_input = input("How many pitchers? (press Enter for ALL): ").strip()
    num_pitchers = int(num_input) if num_input else None
    
    if num_pitchers:
        print(f"\n⚠️  Will scrape {num_pitchers} pitchers")
    else:
        print(f"\n⚠️  Will scrape ALL pitchers (this may take hours!)")
    
    confirm = input("Continue? (y/n): ").strip().lower()
    
    if confirm == 'y':
        scrape_pitchers(int(year), num_pitchers)
    else:
        print("Cancelled")


if __name__ == "__main__":
    main()