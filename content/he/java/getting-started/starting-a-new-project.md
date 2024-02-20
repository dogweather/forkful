---
date: 2024-01-20 18:03:56.128592-07:00
description: "\u05DB\u05E9\u05DE\u05EA\u05D7\u05D9\u05DC\u05D9\u05DD \u05E4\u05E8\u05D5\
  \u05D9\u05E7\u05D8 \u05D7\u05D3\u05E9 \u05D1- Java, \u05D6\u05D4 \u05DB\u05DE\u05D5\
  \ \u05DC\u05E7\u05D7\u05EA \u05D3\u05E3 \u05DC\u05D1\u05DF \u05D5\u05DC\u05D4\u05EA\
  \u05D7\u05D9\u05DC \u05DC\u05E6\u05D9\u05D9\u05E8. \u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05DE\u05E9\u05D4\u05D5 \u05D7\
  \u05D3\u05E9, \u05DC\u05E4\u05EA\u05D5\u05E8 \u05D1\u05E2\u05D9\u05D4, \u05D0\u05D5\
  \ \u05DC\u05DC\u05DE\u05D5\u05D3 \u05DB\u05D9\u05E9\u05D5\u05E8\u05D9\u05DD \u05D7\
  \u05D3\u05E9\u05D9\u05DD."
lastmod: 2024-02-19 22:04:58.352695
model: gpt-4-1106-preview
summary: "\u05DB\u05E9\u05DE\u05EA\u05D7\u05D9\u05DC\u05D9\u05DD \u05E4\u05E8\u05D5\
  \u05D9\u05E7\u05D8 \u05D7\u05D3\u05E9 \u05D1- Java, \u05D6\u05D4 \u05DB\u05DE\u05D5\
  \ \u05DC\u05E7\u05D7\u05EA \u05D3\u05E3 \u05DC\u05D1\u05DF \u05D5\u05DC\u05D4\u05EA\
  \u05D7\u05D9\u05DC \u05DC\u05E6\u05D9\u05D9\u05E8. \u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05DE\u05E9\u05D4\u05D5 \u05D7\
  \u05D3\u05E9, \u05DC\u05E4\u05EA\u05D5\u05E8 \u05D1\u05E2\u05D9\u05D4, \u05D0\u05D5\
  \ \u05DC\u05DC\u05DE\u05D5\u05D3 \u05DB\u05D9\u05E9\u05D5\u05E8\u05D9\u05DD \u05D7\
  \u05D3\u05E9\u05D9\u05DD."
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
---

{{< edit_this_page >}}

## מה ולמה?

כשמתחילים פרויקט חדש ב- Java, זה כמו לקחת דף לבן ולהתחיל לצייר. תוכניתנים עושים את זה כדי ליצור משהו חדש, לפתור בעיה, או ללמוד כישורים חדשים.

## איך לעשות:

כדי להתחיל פרויקט חדש ב-Java, תצטרך תכנה שתומכת בכך, כמו IntelliJ IDEA או Eclipse. הנה דוגמא פשוטה של קוד לפרויקט חדש:

```java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("שלום עולם!");
    }
}
```

כאשר תריץ את הקוד, תראה:

```
שלום עולם!
```

## עיון נוסף:

לפני Maven ו-Gradle, היינו נאלצים לנהל תלותיות ידנית, עם המון כאב ראש. כיום, כלים אלו מקלים עלינו להקים מבנה פרויקט. לדוג', ב- Maven:

```xml
<project>
    <modelVersion>4.0.0</modelVersion>
    <groupId>com.example</groupId>
    <artifactId>myapp</artifactId>
    <version>1.0-SNAPSHOT</version>
</project>
```

כשמשתמשים ב-IDE, הוא לרוב יכניס אותך ישירות לעולם של מבנה פרויקטים מסודר עם כל ההגדרות שנחוצות.

## ראה גם:

- [התיעוד הרשמי של Maven](https://maven.apache.org/guides/index.html)
- [איך לבנות פרויקט עם Gradle](https://gradle.org/guides/#getting-started)
- [תיעוד IntelliJ IDEA ליצירת פרויקטים חדשים](https://www.jetbrains.com/help/idea/creating-and-running-your-first-java-application.html)
