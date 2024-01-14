---
title:                "Python: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-csv.md"
---

{{< edit_this_page >}}

## למה

CSV היא פורמט נתונים תפור המשמש לאחסון ושיתוף נתונים טבלאיים. עם זאת, היא מציעה דרך פשוטה ויעילה להעברת נתונים בין תוכניות ומערכות שונות. ניתן לעבוד עם קבצי CSV באמצעות פייתון בקלות ולשלב אותם עם כלי נתונים אחרים, כגון Excel ו- SQL, מה שמקל על העבודה ומונע טעויות בנתונים.

## איך לעבוד עם CSV בפייתון

### קריאת קובץ CSV

כדי לקרוא נתונים מקובץ CSV בפייתון, ניתן להשתמש בפונקציית `reader()` מהמודול `csv`. ניתן לציין את כתובת הקובץ בתור פרמטר ולהשתמש בלולאה כדי לעבור על הנתונים.

```Python
import csv

with open('file.csv') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    for row in csv_reader:
        # do something with the data
```

### כתיבת נתונים לקובץ CSV

ניתן ליצור קובץ CSV חדש ולכתוב אליו נתונים באמצעות פונקציית `writer()` מהמודול `csv`. ניתן לציין את כתובת הקובץ ואת השדות של הטבלה.

```Python
import csv

with open('new_file.csv', 'w') as csv_file:
    csv_writer = csv.writer(csv_file, delimiter=',')
    csv_writer.writerow(['Name', 'Age', 'City'])
    csv_writer.writerow(['John', '25', 'New York'])
    csv_writer.writerow(['Rachel', '30', 'Los Angeles'])
```

### מיזוג קבצי CSV

פייתון מציעה דרך פשוטה למזג קבצי CSV מספר וליצור קובץ ממוזג יחיד. ניתן להשתמש במודול `csv` כדי ליצור קובץ חדש ולכתוב אליו את הנתונים של מספר הקבצים המזוגים.

```Python
import csv
import glob

with open('merged_file.csv', 'w') as csv_file:
    csv_writer = csv.writer(csv_file, delimiter=',')
    # get all csv files in the current directory
    csv_files = glob.glob('./*.csv')
    # write header row
    csv_writer.writerow(['Name', 'Age', 'City'])
    # loop through each file
    for file in csv_files:
        with open(file, 'r') as csv_file:
            csv_reader = csv.reader(csv_file)
            # skip header row
            next(csv_reader)
            # write data from file