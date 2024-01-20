---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

מציאת אורך של מחרוזת היא בעצם ספירת התווים שמרכיבים את המחרוזת. מתכנתים צריכים לזהות את אורך מחרוזת, למשל, כדי ליישם ולייצר קוד משופר ומתוקף.

## איך עושים את זה?

ניתן למצוא את אורך המחרוזת בשפת Rust באופן פשוט מאוד, באמצעות הפונקציה `len()`.
בהנחה שיש לנו מחרוזת שנקראת "צ'יקו":

```Rust
let chiko = "צ'יקו";
let length = chiko.chars().count();
```

ההדפסה של `length` תחזיר את התוצאה 5.

## צלילה מעמיקה

הפונקציה `len()` בשפת Rust ממומשת כפונקציה שמחזירה את מספר הבייטים במחרוזת ולא את מספר התווים. במקרים בהם התווים הם באותו משקל בינלאומי כמו ASCII, הדבר לא משנה. אך במקרים כאשר מדובר על תווים Unicode כמו בערך "צ'יקו", הרי שהשימוש ב `chars().count()` הוא המתאים, מאחר והוא מאפשר ספירה בהתאם לתווים האקטואליים.

## ראה גם

- התיעוד הרשמי של Rust לגבי מחרוזות [כאן](https://doc.rust-lang.org/book/ch08-02-strings.html)
- פוסט בלוג המתמקד ביחס מדויק של Rust למחרוזות Unicode [כאן](https://www.ameyalokare.com/rust/2017/10/12/rust-str-vs-String.html)