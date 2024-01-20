---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת קובץ טקסט ב-Java זהו תהליך שבו התוכנה מקריאה נתונים מקובץ טקסט קיים. תכנתים משתמשים בזה לעבד נתונים מראש נכתבים, לשמור על המילול הקל ולחסוך זמן בקליטת נתונים.

## איך:

הקוד הבא מדגים איך לקרוא קובץ טקסט ב-Java באמצעות 

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("example.txt"));
            String line = reader.readLine();

            while (line != null) {
                System.out.println(line);
                line = reader.readLine();
            }

            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

פלט, לדוגמה:
```
שורה 1
שורה 2
```

## צלילה עמוקה

מאז שמוגדרים הגרסאות הראשונות של Java, קיימים מספר דרכים לקריאה של קבצי טקסט. השיטה שהוצגה למעלה הייא זו שהשתמשה ב-``BufferedReader`` ו-``FileReader``. קיימות שיטות חלופות כמו השימוש ב-``Scanner`` וב-``FileInputStream``.

אנחנו ב-``BufferedReader`` מכיוון שהיא מציעה יתרונות מבחינת ביצועים וקריאה של שורות שלמות. כמו כן, ``FileReader`` מספקת גישה לקריאה של קבצים באופן קל ומובן, ללא הצורך בהמרה של הדאטה.

בכל מקרה, יש לשים לב לניהול יעיל של משאבים כאשר מתמודדים עם קבצים.

## ראה גם

1. [BufferedReader (Oracle Docs)](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/io/BufferedReader.html)
2. [FileReader (Oracle Docs)](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/io/FileReader.html) 
3. [Code Ranch: Guide to Scanner vs BufferedReader](https://coderanch.com/t/419192/java/java/Scanner-BufferedReader) 
4. [Baeldung: Guide to Java FileInputStream](https://www.baeldung.com/java-file-input-stream)