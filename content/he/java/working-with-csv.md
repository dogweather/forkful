---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"

category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם CSV מובילה לקריאה וכתיבה של נתונים בפורמט פשוט, תוך כדי שמירה על סדר שורות ועמודות. מתכנתים עושים זאת כדי להחליף נתונים בין שירותים שונים ולעבד אותם בקלות.

## איך לעשות:
```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String path = "data.csv";
        String line = "";
        
        try {
            BufferedReader br = new BufferedReader(new FileReader(path));
            
            while ((line = br.readLine()) != null) {
                String[] values = line.split(",");
                // כאן ניתן לעבד כל שורה מהCSV
                System.out.println("שם: " + values[0] + ", גיל: " + values[1]);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
תוצאה:
```
שם: טל, גיל: 30
שם: אביב, גיל: 22
```

## טבילה עמוקה
CSV, או ערכים מופרדים בפסיקים, החל כפורמט בשנות ה-70. פורמטים חלופיים כוללים JSON וXML, אך CSV נשאר נפוץ בזכות פשטותו. בתיקון טעויות או פירוק נתונים, חשוב לשים לב למקרים שבהם ישנם פסיקים, מרכאות או שורות חדשות בתוך ערך. עלולים להיות צורך בספרייה לעיבוד מיוחד כמו Apache Commons CSV או opencsv.

## ראו גם

- Apache Commons CSV: https://commons.apache.org/proper/commons-csv/
- OpenCSV: http://opencsv.sourceforge.net/
- מדריך לCSV בJava: https://www.baeldung.com/java-csv
