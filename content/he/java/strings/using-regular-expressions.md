---
aliases:
- /he/java/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:48.653585-07:00
description: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D1\u05D2'\u05D0\u05D5\u05D5\u05D4 \u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05D9\u05DD \u05DC\u05DA \u05DC\u05D4\u05D2\u05D3\u05D9\u05E8 \u05D3\
  \u05E4\u05D5\u05E1\u05D9\u05DD \u05DE\u05E1\u05D5\u05D9\u05DE\u05D9\u05DD \u05DC\
  \u05D7\u05D9\u05E4\u05D5\u05E9, \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3 \u05D0\u05D5\
  \ \u05DC\u05D0\u05D9\u05DE\u05D5\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D1\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\u05DD \u05DC\u05DE\
  \u05E9\u05D9\u05DE\u05D5\u05EA \u05DB\u05DE\u05D5 \u05E0\u05D9\u05EA\u05D5\u05D7\
  \ \u05E7\u05D1\u05E6\u05D9\u2026"
lastmod: 2024-02-18 23:08:52.697986
model: gpt-4-0125-preview
summary: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D1\u05D2'\u05D0\u05D5\u05D5\u05D4 \u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05D9\u05DD \u05DC\u05DA \u05DC\u05D4\u05D2\u05D3\u05D9\u05E8 \u05D3\
  \u05E4\u05D5\u05E1\u05D9\u05DD \u05DE\u05E1\u05D5\u05D9\u05DE\u05D9\u05DD \u05DC\
  \u05D7\u05D9\u05E4\u05D5\u05E9, \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3 \u05D0\u05D5\
  \ \u05DC\u05D0\u05D9\u05DE\u05D5\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D1\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\u05DD \u05DC\u05DE\
  \u05E9\u05D9\u05DE\u05D5\u05EA \u05DB\u05DE\u05D5 \u05E0\u05D9\u05EA\u05D5\u05D7\
  \ \u05E7\u05D1\u05E6\u05D9\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

ביטויים רגולריים (regex) בג'אווה מאפשרים לך להגדיר דפוסים מסוימים לחיפוש, לעיבוד או לאימות מחרוזות בקוד שלך. מתכנתים משתמשים בהם למשימות כמו ניתוח קבצי לוג, אימות קלט ממשתמשים או חיפוש אחר דפוסים מסוימים בטקסט, מה שמאפשר עיבוד מחרוזות מתוחכם עם מאמץ מינימלי.

## איך לעשות:

התמיכה המובנית של ג'אווה ב-regex מתבצעת בעיקר דרך המחלקות `Pattern` ו-`Matcher` בחבילה `java.util.regex`. הנה דוגמה פשוטה למציאה והדפסה של כל המופעים של מילה במחרוזת, ללא תלות ברישיות:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex is great for parsing. Parsing with regex is powerful.";
        String wordToFind = "parsing";
        
        Pattern pattern = Pattern.compile(wordToFind, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("נמצאה '" + matcher.group() + "' במיקום " + matcher.start());
        }
    }
}
```

פלט:
```
נמצאה 'parsing' במיקום 16
נמצאה 'Parsing' במיקום 31
```

למשימות כמו פיצול מחרוזות, ניתן להשתמש בשיטת `split()` של המחלקה `String` עם regex:

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] languages = text.split(",");
        
        for (String language : languages) {
            System.out.println(language);
        }
    }
}
```

פלט:
```
Java
Python
Ruby
JavaScript
```

כאשר עובדים עם regex בג'אווה, ייתכן שיהיו מקרים שבהם ספרייה חיצונית תוכל להפשיט משימות מורכבות. אחת מהספריות החיצוניות הפופולריות לעבודה עם regex בג'אווה היא `Apache Commons Lang`. היא מציעה כלים כמו `StringUtils` שעושים כמה משימות regex פשוטות יותר. הנה איך להשתמש בה לספירת מופעים של תת-מחרוזת:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex makes text processing easier. Processing text with regex is efficient.";
        String substring = "processing";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' מופיע " + count + " פעמים.");
    }
}
```

לשימוש ב-Apache Commons Lang, יש לכלול אותה בפרויקט שלך. אם אתה משתמש ב-Maven, הוסף תלות זו ל-`pom.xml` שלך:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- בדוק את הגרסה האחרונה -->
</dependency>
```

פלט:
```
'processing' מופיע 2 פעמים.
```
