---
title:                "עבודה עם CSV"
html_title:           "Kotlin: עבודה עם CSV"
simple_title:         "עבודה עם CSV"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## למה

כי קובצי CSV הם דרך פשוטה לאחסן מידע בפורמט שניתן לקריאה על ידי מכנה משותף, מה שהופך אותם לכלי חשוב בפיתוח תוכניות ופרויקטים שונים.

## כיצד לעבוד עם CSV באמצעות קוטלין

כדי להתחיל לעבוד עם קבצי CSV באמצעות קוטלין, כל הצריכה לעשות היא להתקין את ספריית CSV קליינט, המספקת פונקציות שימושיות לקריאה וכתיבה של נתונים מתוך קבצי CSV. לדוגמה:

```Kotlin
// ייבוא ספריית CSV קליינט
import io.github.donmocks.csveasy.Csv

// קריאת קובץ CSV קיים והדפסת הנתונים
val myData = Csv.read("my_csv_file.csv")
println(myData)

// כתיבת נתונים לקובץ CSV חדש
val newCsvFile = Csv.write("new_csv_file.csv", listOf("Name", "Email", "Age"), listOf("John Doe", "johndoe@email.com", "25"))
```

הנתונים שיוצגו בפלט בקוד המופיע לעיל באופן אוטומטי יתאימו לנתוני הקובץ CSV המצויים באותה התבנית.

## מעמקים

לעומת פורמטי קבצים אחרים, קבצי CSV אינם דורשים מודולים מיוחדים או תוכנות ייעודיות לקריאה וכתיבה. הם יכולים להיעבר ולהיפתח על ידי כל תוכנית המשתמשת בסיסמתנים. מכיוון שמרבית הנתונים נמצאים בצורה טבלאית, קבצי CSV מתאימים במיוחד להשתמש בקוד גנרי המאפשר לעבוד עם מכללת שיטים ותנאים בתוך לולאות.

## ראה גם

- [ספריית CSV קליינט](https://github.com/donmocks/csveasy)
- [מדריך לעבוד עם קבצי CSV בקוטלין](https://www.javatpoint.com/kotlin-csv-file)