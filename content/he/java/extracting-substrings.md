---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)

יצירת מחרוזות תתי-מחרוזת היא הליך שבו מאחזרים חלק ממחרוזת מחשב. מתכנתים משתמשים בזה לתעד דינמיקאית את המידע הרלוונטי ממחרוזות גדולות או מורכבות.

## How to: (איך ל:)

הנה הדוגמה הבסיסית לאיך למשוך תת-מחרוזת ממחרוזת ב-Java.

```Java
public class Main {
  public static void main(String[] args) {
    String str = "Hello, World!";
    String substring = str.substring(0, 5);
    System.out.println(substring);
  }
}
```
תוצאה:
```
Hello
```

## Deep Dive: (צלילה עמוקה:)

Java הציגה את `substring()` בהשקה הראשונית. ניתן למנוע 
מקרים של `IndexOutofBound` התרסה אם הגבלות האינדקס נמנעות בהתחלה. חלופה ל-`substring()` היא שימוש במחלקת `StringUtils` מ Apache Commons Lang, אם זמין.

## See Also: (ראה גם:)

- [מסמך Oracle על `substring()`](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int))
- [הספרייה Apache Commons Lang](https://commons.apache.org/proper/commons-lang/)