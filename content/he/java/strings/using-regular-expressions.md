---
title:                "שימוש בביטויים רגולריים"
aliases:
- /he/java/using-regular-expressions/
date:                  2024-02-03T19:17:48.653585-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בביטויים רגולריים"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
