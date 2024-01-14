---
title:                "Gleam: קישור מחרוזות"
simple_title:         "קישור מחרוזות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה:

פרידת ה מחרוזות בשפת למדת יכולה להיות מאוד שימושי בסיטואציות רבות, כולל בתכנות וגם בשימושים יומיומיים כמו בכתיבת הודעות או פוסטים במדיה חברתית. כשמשתמשים בפונקציות כמו ```string.concat()``` או באופרטור ```++```, ניתן לקבל מחרוזות מאוחדות בקלות ובמהירות, מה שיעניק נוחות ויכולת יצירתית של קוד.

## איך לעשות זאת:

בדוגמאות הקוד הבאות נראה כיצד לחבר מחרוזות ב-Gleam בשימוש בפונקציות ובאופרטורים שונים:

```
Gleam module Example {

  pub fn simple_concat() {
    let str1 = "Hello ";
    let str2 = "world";
    let result = string.concat(str1, str2);
    IO.print(result);
  }
  
  pub fn complex_concat() {
    let str1 = "Let's ";
    let str2 = "do ";
    let str3 = "something ";
    let str4 = "amazing!";
    let result = str1 ++ str2 ++ str3 ++ str4;
    IO.puts(result);
  }

}
```

פלט של הקוד הנ"ל ביציאת האופקת יהיה:

```
Hello world
Let's do something amazing!
```

## מה הנעשה במפתח:

פרידת המחרוזות ב-Gleam מבצעת פעולת שמירה (concatenation), שפועלת על שני ארגומנטים ומחזירה מחרוזת אחת מאוחדת. פונקציות כמו ```string.concat()``` מקבלות שני ארגומנטים וביצוע את פעולת השמירה עליהם, בעוד שאופרטור כמו ```++```, מספק דרך יעילה יותר לחבר מחרוזות בלי להשתמש בפונקציות.

## ראה גם:

- [פקודת IO.print](https://hexdocs.pm/gleam_stdlib/Gleam.IO.html#print/1)
- [מדריך לעבודה עם מחרוזות ב-Gleam](https://gleam.run/book/core-modules)