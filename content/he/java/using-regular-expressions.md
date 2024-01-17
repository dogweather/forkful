---
title:                "השתמשות בביטויים רגולריים"
html_title:           "Java: השתמשות בביטויים רגולריים"
simple_title:         "השתמשות בביטויים רגולריים"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

# מה ולמה?
שימוש בביטויים רגילים הוא שיטה נפוצה בתכנות שבה ניתן לחפש ולהתאים מבנים טקסטואליים מסוימים. זהו כלי חזק ומועיל במיוחד עבור תרגום, חלוקת טקסט ליחידות ובדיקת תקינות קלט.

## כיצד להשתמש:
להלן דוגמאות של קוד Java לשימוש בביטויים רגילים והפלט שמתקבל:

```java
String text = "האם אתה משתמש בביטויים רגילים בתכנות?";
String pattern = "האם .+ תכנות\\?";
boolean matches = text.matches(pattern);
System.out.println(matches); // פלט: true
```

```java
String text = "למה לא נכנסתי לתוכנית הפנימית של הקורס?";
String pattern = "(למה|מדוע).*תוכנית.*קורס\\?";
boolean matches = text.matches(pattern);
System.out.println(matches); // פלט: true
```

## טיול עמוק:
ביטויים רגילים פותחו לראשונה בשנת 1951 על ידי ברד קול וקנות ריצ'יי, יצרני שפת התכנות קובול. שימוש בביטויים רגילים יכול להיות אפשרות יעילה מבחינה זמנית עבור בקשות מסוימות, אך ישנן אפשרויות נוספות כמו חיפוש עם מילולות ממולכמות או בידוד תת-מחרוזות עם עצרות (capture groups).

כדי לבדוק את התאמת הביטויים הרגילים שלך ולדעת מה הוא מתאים בפועל, ניתן להשתמש בכלי כמו regex101 או RegexBuddy.

## ראו גם:
- [Oracle אינדקס טכני על ביטויים רגילים](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [ויקיפדיה - ביטוי רגיל](https://en.wikipedia.org/wiki/Regular_expression)
- [ביטויים רגילים בג'אבה](http://tutorials.jenkov.com/java-regex/pattern.html)