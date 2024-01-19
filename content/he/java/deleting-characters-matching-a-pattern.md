---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?

מחיקת תווים מתוך מחרוזת בהתאם לדפוס מיוחד היא פעולה חוזרת ונשנית בתכנות. היא משפרת קוד או נתונים ע"י הסרת תווים לא רלוונטיים, כמו מרווחים ראשיים או מסופיים.

## איך עושים?

דוגמת קוד למחיקת התווים 'a' ו'b' מתוך מחרוזת:

```Java
public class Main {
  public static void main(String[] args) {
    String str = "AaBbCc";
    str = str.replaceAll("[aAbB]", "");
    System.out.println(str);
  }
}
```

הדפסה מדוגמת:

```Java
Cc
```

## פרטים מעמיקים

פעולת ההחלפה של Java משתמשת ברגולריות כדי לזהות דפוסים של תווים. זה נחשב לאפשרות עוצמתית אך יכול להיות גם איטי לעיתים. אלטרנטיבות חלופיות כוללות שימוש במחלקה StringTokenizer או העברה אישית עם שיטת charAt.

## ראה גם

- [Java String class documentation](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html)
- [Oracle's Guide on How to Write Character Patterns](https://docs.oracle.com/javase/tutorial/essential/regex/char_classes.html) 
- [Java StringTokenizer Class](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/StringTokenizer.html)