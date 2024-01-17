---
title:                "עבודה עם קובץ csv."
html_title:           "Java: עבודה עם קובץ csv."
simple_title:         "עבודה עם קובץ csv."
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV היא חלק חשוב מתהליך התכנות. CSV היא פירמוט פשוט לקובץ המכיל נתונים מפורסמים ביותר, ויוצא דופן לצורך עבודה ושמירת נתונים. תוכניות מכילות כמות עצומה של מידע, לכן עבודה עם פירמוט פשוט כזה הוא חובה עבור תכניות רבות.

## איך לעבוד?

קבצי CSV ניתנים לקריאה וכתיבה בעזרת קוד Java באמצעות מספר ספריות. הנה דוגמה של קוד הקורא קובץ CSV ומדפיס את הנתונים שבתוכו:

```Java
// הצהרה על הספריות הנדרשות
import java.io.FileReader;
import java.io.IOException;
import java.util.List;

import au.com.bytecode.opencsv.CSVReader;

public class CSVExample {
  public static void main(String[] args) throws IOException {
    // הגדרת קורא CSV וקריאת הקובץ
    CSVReader reader = new CSVReader(new FileReader("example.csv"));
    // הצגת הנתונים בכל שורה המכילה מערך של תאים
    List<String[]> rows = reader.readAll();
    for (String[] row : rows) {
      for (String cell : row) {
        System.out.print(cell + " ");
      }
      System.out.println();
    }
  }
}
```

כאשר נריץ תוכנית זו נקבל את הפלט הבא:

```
First name Last name Email Address
John Smith john@example.com
Jane Doe jane@example.com
Bob Johnson bob@example.com
```

## חיפוש עמוק

פירמוט ה-CSV נוצר בשנות ה-70 כחלק מהסטנדרט של חברת IBM ואינו מסובך במיוחד. יחד עם זאת, ישנן אלטרנטיבות לפירמוט זה הניתנות לשימוש בתכנות, כגון JSON ו-XML. בכל זאת, קבצי CSV נהנים מפופולאריות רבה ושימושם נמצא בתכניות רבות.

טיפול בקבצי CSV מבוסס על תקנים ספציפיים וקיימים מספיק כדי להתאים לצרכי התכנות. בנוסף, ישנן ספריות רבות המתאימות לצרכי יצירת, קריאה ושמירה של קבצי CSV בקוד Java. ניתן גם לכתוב קוד יעיל יותר עבור ספריות אלה בעצמנו, את המרבעת המבואות של אובייקטים כמו Table או Record.

## ראה גם

- [תיעוד רשמי לספריית opencsv](http://opencsv.sourceforge.net/)
- [תיעוד רשמי לספריית Apache Commons CSV](https://commons.apache.org/proper/commons-csv/)