---
title:                "עבודה עם csv"
html_title:           "Swift: עבודה עם csv"
simple_title:         "עבודה עם csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/working-with-csv.md"
---

{{< edit_this_page >}}

# למה

נסתכל סביבנו ונגלה שה-CSV הוא אחד הפורמטים הנפוצים ביותר לאחסון ועריכת נתונים בתוכניות כמו אקסל ו-NOTE ואינטרנט. כארגז כלים פשוט ויעיל לעבודה עם נתונים מורכבים, יש לו המון יתרונות המאפשרים להשתמש בו כדי למזער את זמן העבודה ולהעלות את היציבות.

# איך לנתח קבצי CSV עם Swift?

אלו הן שלושת הצעדים הראשונים של הכיתוב המופשט:

1. כגון, נותח קובץ CSV וקרא אותו באמצעות הפעולות של `NSArray` ו- `NSData`כדי לקרוא אותו יחד. ניתן למצוא ידנית יותר מידע על אופן העבודה לעיתים קרובות.

2. כמו כן, ניתן להשתמש בספריית Swift CSV כדי ליצור פתרון יעיל יותר. יש פעמים שיש למצוא אל מול זה כל הזמן את הפעולות הנדרשות עד כדי כתב CSV. אני ממליץ לנסות להשתמש בו במספר מקומות הטומן, כמו הנתונים שנתגלו עד כה.

כאזור המסומן `CSVData`. פתקים של תיקיות חלקות הימו יכולים לתת פתרון רגיל לקוראי הקובץ, נערכים להתאים ולנתח תוך כדי הבחירה באמצעות כלים אלו.

```Swift
// ניתוח קובץ CSV עם CSVData
let csvDataURL = URL(fileURLWithPath: "my_csv_file.csv") //במקור משתמשים בקובץ
let text = try String(contentsOf: csvDataURL) // קריאת הקובץת למחרוות עמודות
let csv: CSVData = try CSVData(text: text, hasHeaders: true)
// פתרון הכתב במפתחים
// אנו יכולים כעת לנתח את הנתונים בספרית Swift CSV.
// כאזור המסומן `CSVData`.
```

# יצירת יכולות עמודות בתצוגה מ