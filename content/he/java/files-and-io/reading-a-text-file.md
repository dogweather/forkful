---
date: 2024-01-20 17:54:59.202122-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Java 17 \u05DE\u05E6\
  \u05D9\u05D2 \u05D3\u05E8\u05DB\u05D9\u05DD \u05E0\u05D5\u05D7\u05D5\u05EA \u05DC\
  \u05E7\u05E8\u05D5\u05D0 \u05E7\u05D5\u05D1\u05E6\u05D9 \u05D8\u05E7\u05E1\u05D8\
  . \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4."
lastmod: '2024-03-13T22:44:39.159986-06:00'
model: gpt-4-1106-preview
summary: "Java 17 \u05DE\u05E6\u05D9\u05D2 \u05D3\u05E8\u05DB\u05D9\u05DD \u05E0\u05D5\
  \u05D7\u05D5\u05EA \u05DC\u05E7\u05E8\u05D5\u05D0 \u05E7\u05D5\u05D1\u05E6\u05D9\
  \ \u05D8\u05E7\u05E1\u05D8."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 22
---

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
