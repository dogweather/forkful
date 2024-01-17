---
title:                "עבודה עם קבצי CSV"
html_title:           "Kotlin: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

שלום לכולם! במאמר זה נדבר על עבודה עם קבצי CSV בשפת Kotlin. אני ישתדל להיות קצר ולהחיל על כם סגנון ספציפי לשפת כתיבה של בטלן.

## מה ולמה?

עבודה עם קבצי CSV היא תהליך בו נטען נתונים מקובץ טקסט ומטבלת נתונים בפורמט שנקרא "פצול על ידי פסיקים". תהליך זה נפוץ מאוד בעולם התכנות ומשמש ככלי עבור פירוט וקריאה של נתונים בפורמט נוח וקל להבנה.

## איך לבצע ניתוח קבצי CSV בשפת Kotlin?

בשפת Kotlin יש לנו כמה אפשרויות לתכנות עבור ניתוח קבצי CSV. נביא כאן שני דוגמאות שיכולות לעזור לנו בתהליך זה:

```kotlin
// דוגמא ראשונה לניתוח קובץ CSV
val csvFile = File("data.csv").readLines()
 for (line in csvFile) {
     val values = line.split(",")
     val firstName = values[0]
     val lastName = values[1]
     val age = values[2].toInt()
     println("שם מלא: $firstName $lastName, גיל: $age")
 }
```

בדוגמא הראשונה, אנו קוראים את הקובץ ומחלקים את השורות לפי פסיקים. לאחר מכן, אנו משתמשים במשתנים על מנת לקבל את הערכים הרלוונטיים מהשורה ולהדפיס אותם.

```kotlin
// דוגמא שנייה לניתוח קובץ CSV עם הספרייה "OpenCSV"
val reader = CSVReaderBuilder(FileReader("data.csv")).build()
val csvRows = reader.readAll()
for (row in csvRows) {
    println("שם מלא: ${row[0]} ${row[1]}, גיל: ${row[2]}")
}
```

בדוגמא השנייה, אנו משתמשים בספרייה שנקראת OpenCSV בכדי לעזור לנו בניתוח הקובץ. הספרייה מספקת כמה כלים שימושיים לניתוח שורות בקובץ ולהדפסתם.

## נכנסים עומק לניתוח קבצי CSV

אם תרצו ללכת עומק יותר בעניין, ניתן לציין כי ניתוח קבצי CSV נפוץ מאוד גם במסגרת ניתוח נתונים והתמשכות לנתונים. כמו כן, קיימים כלים נוספים שיסייעו בניהול וניתוח קבצי CSV, כגון הספרייה "Apache Commons CSV" וספריית ה-Java "opencsv".

בנוסף, בכדי לשפר את ביצועי התכנית, ניתן להשתמש במספר שיטות לשימוש בקבצי CSV, כמו קריאת הקובץ כמאפיין סטטי או לשמור את הנתונים בזיכרון במקום לקרוא את הקובץ כל פעם מחדש.

## רקע היסטורי, אפשרויות נוספות ופירוט טכני לעבודה עם קבצי CSV

לכתיבת קבצי CSV יש רקע היסטורי מאוד עשיר ומתפתח. הפורמט התחיל להתפתח בימי ה-SAS ל