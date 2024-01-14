---
title:                "Rust: פירוק HTML"
simple_title:         "פירוק HTML"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/parsing-html.md"
---

{{< edit_this_page >}}

## למה

HTML היא שפת תכנות נפוצה ביותר המשמשת ליצירת דפי אינטרנט. כך שמןדיפס לאינטרנט לעיתים קרובות דורשת לנו לעבוד עם קוד HTML. זה כאשר כלי פיתוח כמו Rust נכנסים לתמוך.

## איך לעשות

כאשר מדובר בזיהוי והצגת קוד HTML, כלי פיתוח כמו Rust יכול להיות מועיל ביותר. בכתבה הזו, אני אדגים כמה פרטים על איך להשתמש בכלי התכנות הזה כדי לפענח ולתצוגה דף אינטרנט.

כדי להתחיל, נגש למטרה שלנו באמצעות גרסה פשוטה של HTML כמו דוגמא. בדוגמא הזאת, אני משתמש בדף פשוט המכיל שורת תמונה עם הכתובת המלאה שלה.

```Rust
use std::fs;

fn main() {
  let html = fs::read_to_string("page.html").unwrap();
  let image_tag = "<img src='/image.jpg'>";
  // תפס תג  HTML
  let start_tag = html.find(image_tag).unwrap();
  let end_tag = html[start_tag..].find("/>").unwrap() + start_tag;
  let tag = &html[start_tag..=end_tag];

  // נפרס תמונת הכתובת מהתג מצד שמאל
  let url_start = tag.find("'").unwrap() + 1;
  let url_end = tag[url_start..].find("'").unwrap() + url_start;
  let url = &tag[url_start..=url_end];

  println!("הכתובת המלאה של התמונה היא: {}", url);
}
```
כאן, אנו קוראים את תוכן הדף HTML בעזרת פונקציית "fs::read_to_string" שמקבלת את השם של הקובץ כפרמטר. נשתמש במיתוג כדי לאתר את התג של התמונה ולפענח את הכתובת המלאה שלה. החלק הכי חשוב כאן הוא תפיסת התג המכיל את הכתובת והכתיבה שלה מתוך תגובת התוכן של הדף HTML.

המגבלה העיקרית של הגישה זו היא שהיא עובדת רק עם דפי HTML פשוטים בלבד ולא יפתח תפעולות מורכבות יותר כמו CSS או