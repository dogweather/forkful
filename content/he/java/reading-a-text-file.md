---
title:                "קריאת קובץ טקסט"
date:                  2024-01-20T17:54:59.202122-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט בג'אווה זה פשוט לשלוף נתונים מקובץ טקסט. מתכנתים עושים את זה כי לעיתים קרובות הנתונים שלהם, כמו קונפיגורציות ונתונים לעיבוד, נמצאים בקבצים.

## איך לעשות:
Java 17 מציג דרכים נוחות לקרוא קובצי טקסט. הנה דוגמה:
```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class TextFileReader {
    public static void main(String[] args) {
        try {
            Path filePath = Path.of("example.txt");
            List<String> lines = Files.readAllLines(filePath);
            lines.forEach(System.out::println);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
פלט לדוגמה:
```
שורה ראשונה של טקסט
שורה שנייה של טקסט
```
## צלילה לעומק:
בעבר, מתכנתים של ג'אווה נאלצו להשתמש ב-`FileReader` ו-`BufferedReader` שדרש כתיבת יותר קוד. השיטות שהוצגו למעלה, כמו `Files.readAllLines`, הן חלק מה-NIO (New Input/Output), נוסף ב-Java 7 לעבודה קלה יותר עם קבצים. יש גם אלטרנטיבות כמו `Files.readString` או שימוש ב-`Scanner` לגמישות.

## גם כדאי לראות:
- [Documentation for java.nio.file.Files](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html)
- [Oracle's tutorial on File I/O](https://docs.oracle.com/javase/tutorial/essential/io/)
- [Java 17 API Specification](https://docs.oracle.com/en/java/javase/17/docs/api/index.html)