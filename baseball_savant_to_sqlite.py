"""
Baseball Savant Scraper with SQLite Output
Writes pitch data directly to SQLite database for easy SQL analysis in RStudio
"""

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
import time
import os
import glob
import sqlite3
import pandas as pd

def scrape_to_sqlite(year, num_pitchers=None, db_path=None):
    """
    Scrape Baseball Savant data directly to SQLite database
    
    Args:
        year: Season year
        num_pitchers: Number to scrape (None = all)
        db_path: Path to SQLite database (default: ~/Downloads/baseball_savant_{year}.db)
    """
    # Setup database
    if db_path is None:
        downloads_dir = os.path.expanduser("~/Downloads")
        db_path = os.path.join(downloads_dir, f"baseball_savant_{year}.db")
    
    print(f"\n{'='*60}")
    print(f"Baseball Savant Scraper - {year}")
    print(f"{'='*60}")
    print(f"SQLite Database: {db_path}")
    
    # Connect to database
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()
    
    # Create pitchers table
    cursor.execute("""
        CREATE TABLE IF NOT EXISTS pitchers (
            pitcher_id INTEGER PRIMARY KEY,
            pitcher_name TEXT,
            year INTEGER,
            total_pitches INTEGER,
            first_scraped TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
    """)
    
    
    
    conn.commit()
    
    # Temp directory for CSV downloads
    temp_dir = os.path.join(os.path.expanduser("~/Downloads"), f"temp_baseball_data_{year}")
    os.makedirs(temp_dir, exist_ok=True)
    
    # Setup Chrome
    chrome_options = Options()
    chrome_options.binary_location = "/Applications/Google Chrome 2.app/Contents/MacOS/Google Chrome"
    
    prefs = {
        "download.default_directory": temp_dir,
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
        
        # Count pitchers
        table = driver.find_element(By.TAG_NAME, "table")
        rows = table.find_elements(By.TAG_NAME, "tr")
        data_rows = rows[1:]
        
        actual_pitcher_rows = []
        for row in data_rows:
            cells = row.find_elements(By.TAG_NAME, "td")
            if len(cells) > 1:
                actual_pitcher_rows.append(row)
        
        total_pitchers = len(actual_pitcher_rows)
        print(f" Found {total_pitchers} pitchers with data")
        
        if num_pitchers is None:
            num_pitchers = total_pitchers
        else:
            num_pitchers = min(num_pitchers, total_pitchers)
        
        print(f"\nScraping {num_pitchers} pitchers...")
        print(f"{'='*60}\n")
        
        start_time = time.time()
        successful = 0
        failed = 0
        pitcher_index = 0
        
        for i in range(num_pitchers):
            while pitcher_index < len(data_rows):
                if successful > 0 and successful % 10 == 0:
                    percent = int((successful / num_pitchers) * 100)
                    elapsed_time = time.time() - start_time
                    avg_time = elapsed_time / successful
                    remaining = (num_pitchers - successful) * avg_time
                    eta_minutes = int(remaining / 60)
                    print(f"\n{'='*60}")
                    print(f"Progress: [{successful}/{num_pitchers}] {percent}% complete")
                    print(f"Elapsed: {int(elapsed_time/60)} min | ETA: ~{eta_minutes} min")
                    print(f"Database: {successful} pitchers imported")
                    print(f"{'='*60}\n")
                
                print(f"[{successful + failed + 1}/{num_pitchers}] Processing pitcher...")
                
                try:
                    driver.get(url)
                    time.sleep(5)
                    
                    driver.execute_script("window.scrollTo(0, 0);")
                    time.sleep(1)
                    
                    table = driver.find_element(By.TAG_NAME, "table")
                    rows = table.find_elements(By.TAG_NAME, "tr")
                    data_rows = rows[1:]
                    
                    if pitcher_index >= len(data_rows):
                        break
                    
                    target_row = data_rows[pitcher_index]
                    driver.execute_script("arguments[0].scrollIntoView({block: 'center'});", target_row)
                    time.sleep(3)
                    
                    cells = target_row.find_elements(By.TAG_NAME, "td")
                    
                    if len(cells) <= 1:
                        pitcher_index += 1
                        continue
                    
                    if len(cells) <= 2:
                        pitcher_index += 1
                        continue
                    
                    pitcher_name_cell = cells[2]
                    pitcher_name = pitcher_name_cell.text.strip()
                    
                    if not pitcher_name:
                        pitcher_index += 1
                        continue
                    
                    print(f"  Pitcher: {pitcher_name}")
                    
                    # Find graphs div
                    graphs_div = None
                    for cell in cells:
                        try:
                            potential_div = cell.find_element(By.CLASS_NAME, "graphs")
                            if potential_div.text.strip() == "Graphs" and potential_div.is_displayed():
                                graphs_div = potential_div
                                break
                        except:
                            continue
                    
                    if not graphs_div:
                        pitcher_index += 1
                        failed += 1
                        continue
                    
                    files_before = set(glob.glob(os.path.join(temp_dir, "*.csv")))
                    
                    print("  Clicking Graphs...")
                    driver.execute_script("arguments[0].click();", graphs_div)
                    time.sleep(4)
                    
                    popup = None
                    try:
                        popup = driver.find_element(By.CLASS_NAME, "chart-options")
                        if not popup.is_displayed():
                            popup = None
                    except:
                        popup = None
                    
                    if not popup:
                        pitcher_index += 1
                        failed += 1
                        continue
                    
                    print("  Downloading CSV...")
                    try:
                        csv_div = popup.find_element(By.CLASS_NAME, "csv")
                        driver.execute_script("arguments[0].click();", csv_div)
                    except:
                        pitcher_index += 1
                        failed += 1
                        continue
                    
                    # Wait for download
                    new_file = None
                    for attempt in range(10):
                        time.sleep(1)
                        files_after = set(glob.glob(os.path.join(temp_dir, "*.csv")))
                        new_files = files_after - files_before
                        if new_files:
                            new_file = list(new_files)[0]
                            break
                    
                    if not new_file:
                        pitcher_index += 1
                        failed += 1
                        continue
                    
                    # Import to database
                    print("  Importing to database...")
                    try:
                        df = pd.read_csv(new_file)
                        
                        if len(df) == 0:
                            print("    CSV is empty")
                            os.remove(new_file)
                            pitcher_index += 1
                            failed += 1
                            continue
                        
                        # Get pitcher ID from the CSV
                        pitcher_id = None
                        if 'pitcher' in df.columns and not df['pitcher'].isna().all():
                            pitcher_id = int(df['pitcher'].iloc[0])
                        
                        if not pitcher_id:
                            print("    No pitcher ID in CSV")
                            os.remove(new_file)
                            pitcher_index += 1
                            failed += 1
                            continue
                        
                        # Insert pitcher info
                        cursor.execute("""
                            INSERT OR REPLACE INTO pitchers (pitcher_id, pitcher_name, year, total_pitches)
                            VALUES (?, ?, ?, ?)
                        """, (pitcher_id, pitcher_name, year, len(df)))
                        
                        # Import pitch data - pandas will handle column mapping
                        df.to_sql('pitches', conn, if_exists='append', index=False)
                        
                        conn.commit()
                        print(f"  Imported {len(df)} pitches to database")
                        
                    except Exception as import_error:
                        print(f"    Import error: {import_error}")
                        # Continue anyway 
                    
                    # Clean up temp file
                    try:
                        os.remove(new_file)
                    except:
                        pass
                    
                    successful += 1
                    pitcher_index += 1
                    break
                    
                except Exception as e:
                    print(f"   Error: {e}")
                    failed += 1
                    pitcher_index += 1
                    break
        
        driver.quit()
        
        total_time = time.time() - start_time
        
        # Final summary
        cursor.execute("SELECT COUNT(*) FROM pitchers")
        total_pitchers_db = cursor.fetchone()[0]
        
        cursor.execute("SELECT COUNT(*) FROM pitches")
        total_pitches_db = cursor.fetchone()[0]
        
        print(f"\n{'='*60}")
        print("SCRAPING COMPLETE")
        print(f"{'='*60}")
        print(f" Successful: {successful}")
        print(f" Failed: {failed}")
        print(f" Total time: {int(total_time/60)} min {int(total_time%60)} sec")
        print(f"\n SQLite Database:")
        print(f"   Pitchers: {total_pitchers_db}")
        print(f"   Pitches: {total_pitches_db:,}")
        print(f"   File: {db_path}")
        
        conn.close()
        
        # Clean up temp directory
        import shutil
        shutil.rmtree(temp_dir, ignore_errors=True)
        
        return db_path
        
    except Exception as e:
        print(f"\n Fatal error: {e}")
        conn.close()
        return None


def main():
    print(" Baseball Savant Scraper → SQLite")
    print("="*60)
    
    year = input("Which year? (e.g., 2025): ").strip()
    if not year:
        print("No year entered")
        return
    
    num_input = input("How many pitchers? (press Enter for ALL): ").strip()
    num_pitchers = int(num_input) if num_input else None
    
    if num_pitchers:
        print(f"\n  Will scrape {num_pitchers} pitchers")
    else:
        print(f"\n  Will scrape ALL pitchers")
    
    confirm = input("Continue? (y/n): ").strip().lower()
    
    if confirm == 'y':
        db_path = scrape_to_sqlite(int(year), num_pitchers)
        
        if db_path:
            print("\n Done! Ready for RStudio:")
            print(f"\nIn RStudio, connect with:")
            print(f'  con <- dbConnect(RSQLite::SQLite(), "{db_path}")')
            print(f'\nThen query with:')
            print(f'  dbGetQuery(con, "SELECT * FROM pitchers LIMIT 10")')
            print(f'  dbGetQuery(con, "SELECT * FROM pitches WHERE pitcher_id = 123456")')
    else:
        print("Cancelled")


if __name__ == "__main__":
    main()