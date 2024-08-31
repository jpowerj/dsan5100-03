# Fixes the <img> tags in the generated html files, which point to /figure-html/
# when they should point to /figure-revealjs/
import glob
import os

docs_path = os.path.join(".","docs","463a01339cf0f456ba54a1849df50d1a22c247e3")
weeks_glob_str = os.path.join(docs_path, "w??")

def load_file(fpath):
    with open(fpath, 'r', encoding='utf-8') as infile:
        file_text = infile.read()
    return file_text

def save_to_file(text, fpath):
    with open(fpath, 'w', encoding='utf-8') as outfile:
        outfile.write(text)

def main():
    week_dirs = sorted(glob.glob(weeks_glob_str))
    #print(week_dirs)
    for cur_week_dir in week_dirs:
        cur_notes_fpath = os.path.join(cur_week_dir, "index.html")
        if not os.path.isfile(cur_notes_fpath):
            # This happens when we're just previewing the slides, so it hasn't
            # created an index.html file yet
            continue
        #html_glob_str = os.path.join(cur_week_dir, "*.html")
        orig_notes_html = load_file(cur_notes_fpath)
        #print(orig_notes_html)
        # Perform replacement
        cleaned_html = orig_notes_html.replace("index_files/figure-html", "index_files/figure-revealjs")
        # And save
        save_to_file(cleaned_html, cur_notes_fpath)
    print("Updated index.html files for each week")

if __name__ == "__main__":
    main()
