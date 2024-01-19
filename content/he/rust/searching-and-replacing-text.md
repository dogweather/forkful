---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפה של טקסט הם פעולות שמאפשרות לנו למצוא מחרוזת מסוימת בתוך טקסט ולהחליף אותה במחרוזת אחרת. תכנתים משתמשים בכך באופן נרחב לשיפור יעילות הקוד שלהם ולתיקון שגיאות.

## איך:
חפש והחלף ב-Rust ניתן להשיג באמצעות ביטויים רגולריים. נדע איך להשתמש בפקודה `replace()` במערכת הביטוי הרגולרי אשר מקבלת שני קלטים; מה לחפש ועם מה להחליף:

```Rust
use regex::Regex;

let s = "אני אוהב את Rust";
let re = Regex::new("Rust").unwrap();
let result = re.replace_all(&s, "Python");

println!("{}", result);  // "אני אוהב את Python"
```

## שיעור עמיק:
1. מסגרת הטקסט: היסטוריה - בלייבררי ה - [regex](https://docs.rs/regex/1.3.9/regex/) של Rust, פעולת החיפוש וההחלפה הושתמשה כדי לשפר באופן משךוני את אפשרויות מניפולציות המחרוזת שגואה והשיפור שהיא מציעה.  
  
2. אלטרנטיבות - ספריות אחרות נמשכות מה [str של ספרייה למיניפולציות על מחרוזות](https://doc.rust-lang.org/std/str/) כ - `replace()`. Ζה מצריך יותר עבודה אבל מאפשר שליטה גדולה יותר.

3. מימוש - Rust מממשת שיטת חיפוש והחלפה של מחרוזות ישירות למערכת ביטוי הרגולרי שלה. זה אומר שאפשר להכניס כל ביטוי רגולרי והמערכת תדע להסתדר איתו.  

## ראו גם:
מידע נוסף על שימוש בביטויים רגולריים ב-Rust אפשר למצוא [כאן](https://docs.rs/regex/1.3.9/regex/).
מידע נוסף על מניפולציות מחרוזת ב-Rust אפשר למצוא [כאן](https://doc.rust-lang.org/book/ch08-02-strings.html).
אגם פתרונות, יוכל לשמש את הqqq-לואי regex של Rust ראו [כאן](https://rust-lang-nursery.github.io/rust-cookbook/strings/modify_strings.html).