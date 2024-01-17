---
title:                "מציאת אורך של מחרוזת"
html_title:           "Rust: מציאת אורך של מחרוזת"
simple_title:         "מציאת אורך של מחרוזת"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# מה ולמה?
תכנותנים לעתים קרובות נדרשים לקבוע את אורך התווים במחרוזת. לדוגמה, כאשר פעולה מסוימת מצריכה תווים מסוימים לשימוש או כאשר נדרש להשוות מחרוזות באורך זהה. כדי לקבל את האורך של מחרוזת מסוימת, נדרש לבצע פעולת "מציאת אורך של מחרוזת".

# איך לעשות זאת:
דוגמאות קוד ופלט נתונים:
```Rust
let name = "רוסט";
let length = name.len();

println!("אורך המחרוזת של {} הוא {}", name, length);
```

פלט:
```
אורך המחרוזת של רוסט הוא 4
```

# כיול עמוק:
פעולת "מציאת אורך של מחרוזת" היא חלק חשוב מתכנות ונמצאת כבר בשפות תכנות אחרות כמו C ו-Java. ב-Rust, ניתן למצוא את האורך של מחרוזת באמצעות הפעולה len(), או על ידי ספירת התווים במחרוזת באמצעות פעולת chars(). כדי למצוא את האורך הכולל של מחרוזת עם תווים ביניהם, ניתן להשתמש בפעולה utf8_len().

# ראו גם:
למידע נוסף על הפעולות השונות למציאת אורך של מחרוזת ופעולות אחרות ב-Rust, ניתן לעיין בקישורים הבאים:
- [`len()` ו- `chars()`](https://doc.rust-lang.org/std/string/struct.String.html)
- [`utf8_len()`](https://doc.rust-lang.org/std/str/fn.utf8_len.html)
- [ טכניות לספירת תווים במחרוזת](https://doc.rust-lang.org/std/str/fn.chars.html)
- [שפת תכנות Rust הרשמית](https://www.rust-lang.org/)