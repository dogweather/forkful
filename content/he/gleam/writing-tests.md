---
title:                "כתיבת בדיקות"
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות היא תהליך שבו אנחנו בודקים שהקוד שלנו עובד כמו שצריך. תוכניתנים עושים את זה כדי למנוע שגיאות, ולשמור על קוד איכותי ויציב.

## איך לעשות:
ב-Gleam, בדיקות נכתבות בעזרת מודול ה-test. הנה דוגמה פשוטה:
```gleam
import gleam/should
import my_module

pub fn test_addition() {
  my_module.add(1, 2)
  |> should.equal(3)
}
```
פלט דוגמה:
```
test my_module.test_addition ... ok
```

## עיון עמוק
בדיקות התקיימו תמיד, אבל המודרניזציה שלהן בצורה של TDD (Test-Driven Development) הפכה אותן לנורמה. ישנם אלטרנטיבות כמו תחכום מובנה (BDD), אבל ב-Gleam משתמשים בפונקציונליות המובנית. דטלים טכניים כמו תנאי קדם ותנאי אחרי, מודוליה ומחיקת מצב עולמי חשובים בניהול בדיקות.

## ראה גם
- [מדריך התחלתי ל-Gleam](https://gleam.run/book)
- [רפוסיטורי Gleam על GitHub](https://github.com/gleam-lang/gleam)