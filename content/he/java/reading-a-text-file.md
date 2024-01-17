---
title:                "קריאת קובץ טקסט"
html_title:           "Java: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

כתיבת יישומי תוכנה היא עיסוק מהתובענים שנתנוהו. אחד המשימות הנפוצות שמתבקשות ממנו הוא קריאת קבצי טקסט. מה זה אומר ולמה המתכנתים עושים את זה?

### מה ולמה?

קריאת קובץ טקסט היא תהליך שבו התוכנית שלך קוראת ומעבדת מידע מקובץ טקסט. זה יכול להיות טקסט פשוט, כגון מידע ממסמך מילה, או תוכן פורמטים יותר מורכבים כגון נתוני XML או JSON. תפקיד הקורא קובץ הטקסט הוא לקרוא את הנתונים מהקובץ ולהעביר אותם לתוכניתך כך שתוכל לעבוד איתם.

## איך לעשות זאת:

כדי לקרוא קובץ טקסט בקוד ג'אבה, צריך להשתמש באובייקט שנקרא "קורא מקובץ" (FileReader) ובאובייקט "קורא תווים" (BufferedReader). בקוד נמצא דוגמא לאיך להשתמש בשני האובייקטים הללו כדי לקרוא קובץ טקסט ולהדפיס את התוכן שלו:

```java
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class ReadTextFile {
    public static void main(String[] args) {
        //פתיחת קובץ טקסט לקריאה
        File file = new File("myFile.txt");
        
        //הגדרת אובייקט לקריאת קובץ טקסט
        BufferedReader reader = null;
        
        //נסה לקרוא את הקובץ ולהדפיס את התוכן שלו
        try {
            reader = new BufferedReader(new FileReader(file));
            
            String currentLine;
            
            //קריאת כל שורה בקובץ והדפסתה
            while ((currentLine = reader.readLine()) != null) {
                System.out.println(currentLine);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            //סגירת הקובץ והמשאבים שלו
            try {
                reader.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
```

### עומק אינו מחובר

קריאת קובץ טקסט היא טכניקה נפוצה וחשובה ביצירת תוכניות תוכנה. היא שימושית כאשר נצטרך לעבוד עם קבצים שמכילים מידע סטטי או לקרוא נתונים מפורמטים מסוימים. קריאת קובץ טקסט היא גם חלק מחשוב תופעות כמו יצירת מילון טקסט או מתן תחזוקה לנתונים בקובץ.

## ראה גם:

כדי ללמוד עוד על קריאת קובץ טקסט בג'אבה ועל האובייקטים המנוונים, נא לעיין במדריכים הבאים:

- [תיעוד רשמי של Java - FileReader](https://docs.oracle.com/javase/8/docs/api/java/io/FileReader.html)
- [תיעוד רשמי של Java - BufferedReader](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [מדריך קריאה וכתיבת קבצים בג'אבה](https://www.geeksforgeeks.org/different-ways-reading-text-file-java/)