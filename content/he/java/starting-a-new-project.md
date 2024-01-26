---
title:                "התחלת פרויקט חדש"
date:                  2024-01-20T18:03:56.128592-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"
programming_language: "Java"
category:             "Java"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/starting-a-new-project.md"
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
