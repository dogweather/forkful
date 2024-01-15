---
title:                "עבודה עם קובץ CSV"
html_title:           "Python: עבודה עם קובץ CSV"
simple_title:         "עבודה עם קובץ CSV"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-csv.md"
---

{{< edit_this_page >}}

## למה csv?
קבצי CSV הם נפוצים מאוד בעולם התכנות ומשמשים לאחסון וקריאת נתונים בפורמט טקסט פשוט. המאמר הזה ילמד אותך איך לעבוד עם קבצי CSV בשפת פייתון ויתן לך כלים לשליטה על נתונים בצורה יעילה ונוחה.

## איך לעבוד עם CSV בפייתון
כדי להתחיל לעבוד עם קבצי CSV בפייתון עליך לייבא את המודול "csv". באמצעות הפונקציה "reader" ניתן לקרוא נתונים מקובץ CSV ולהציג אותם במבנה של רשימות. ניתן גם להשתמש בפונקציה "writer" כדי לכתוב נתונים לקובץ CSV בצורה מקומפקטית ומועילה. ניתן לראות דוגמאות קוד במסגרת הערכה הזו.

```Python
import csv

# פותח קובץ CSV לקריאה ומחזיר אובייקט
with open('file.csv', 'r') as csvfile:
    # מאתחל קורא פייתון עבור הקובץ
    reader = csv.reader(csvfile)

    # נקרא את הנתונים בצורה של רשימות
    for row in reader:
        print(row)
        
# פותח קובץ CSV לכתיבה ומחזיר אובייקט
with open('file.csv', 'w') as csvfile:
    # מאתחל כותב פייתון עבור הקובץ
    writer = csv.writer(csvfile)

    # כותב את הנתונים לקובץ בפורמט תאים
    writer.writerow(['Name', 'Age', 'Location'])
    writer.writerow(['John', '30', 'New York'])
```

פלט של הקוד הזה הייחודי יכול להיות:

```
['Name', 'Age', 'Location']
['John', '30', 'New York']
```

## מעמיקים בעבודה עם CSV
ניתן לשחק עם המודול "csv" כדי לעבוד עם נתונים בכוחות נוספים כמו פונקציות למניפולציה, איתות וטיפול בשגיאות. כדי ללמוד עוד על היכולות של מודול ה-"csv" ניתן לבקר במסמ