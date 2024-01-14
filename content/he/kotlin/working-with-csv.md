---
title:                "Kotlin: עבודה עם קובץ CSV"
simple_title:         "עבודה עם קובץ CSV"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## למה

CSV היא פורמט נתונים פשוט ונפוץ ביותר שנמצא בשימוש רב בעולם התכנות. יתרונות השימוש ב-CSV כוללים קריאות, פרקטיות ויכולת לכלול נתונים מהירים להישאר מחוברים בכל מקום ולאורך זמן. אם אתה מחפש דרך קלה לאחסן ולנהל נתונים שלך בפורמט מאורגן וקריא, עבודה עם CSV יכולה להיות הפתרון המושלם עבורך.

## כיצד לעבוד עם CSV ב-Kotlin


בכדי לעבוד עם CSV ב-Kotlin, נצטרך להתעסק עם שני מחלקות חשובות: "BufferedReader" ו-"CSVReader". באמצעות "BufferedReader" נקרא את הקובץ המכיל את הנתונים שלנו ונכין אותו לספרייה של "CSVReader". לאחר מכן, נוכל לעבוד עם הנתונים כמו בדוגמאות הבאות:

```kotlin
// ייבוא חבילה שלתוך CSVReader
import com.opencsv.CSVReader

// main function
fun main(args : Array<String>) {
    // פתיחת קובץ CSV באמצעות BufferedReader
    val reader = BufferedReader(FileReader("file.csv"))
    // הכנת הנתונים לספריית CSVReader
    val csvReader = CSVReader(reader)
    // קריאת כל שורה בקובץ
    var nextLine: Array<String>?;
    while (csvReader.readNext() != null) {
        nextLine = csvReader.readNext()
        // הדפסת ערכי כל שורה בקובץ
        for (i in 0..nextLine.size-1) {
            print(nextLine.get(i) + "\t");
        }
        println(); 
    } 
    // סגירת ספריית CSVReader
    csvReader.close()
}
```

תוצאת התקנת קוד זה היא לקבל נתונים מסודרים בפורמט קל לקריאה, כמו בדוגמאות ההדפסה הבאות:

```
Name				Age				Gender
John Smith			25				Male
Jane Doe			30				Female
```

## לחקור עומק עם CSV

עבודה עם CSV יכולה להיות מאתגרת בכמה תחומים, כגון עבודה עם קבצים גדולים או עם נושאים רבים. כדי לקבל ניסי