---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Rust: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך עתידי ועברי משתמש במתמטיקה כדי להגיע לתאריך מסוים מנקודת הזמן הנוכחית. זו פעולה חיונית שמתכנתים עושים למגוון יישומים, כולל הקצאת משאבים, תזמון משימות, ומעקב אחר מידע היסטורי.

## איך לעשות:
אנחנו נבצע את זה באמצעות העזר של החבילה `chrono` ב-Rust.

התקינו את `chrono` דרך Cargo:
```
[dependencies]
chrono = "0.4"
```

החישוב של תאריך עתידי:
```Rust
use chrono::{Duration, Utc};

fn main() {
    let now = Utc::now();
    let future = now.checked_add_signed(Duration::days(30)).unwrap();
    println!("{}", future);
}
```

החישוב של תאריך מהעבר:
```Rust
use chrono::{Duration, Utc};

fn main() {
    let now = Utc::now();
    let past = now.checked_sub_signed(Duration::days(30)).unwrap();
    println!("{}", past);
}
```

## עומק השקיעה
החישוב של תאריכים עתידיים ומהעבר הוא בעצם טכניקה מתמטית והשפת התכנות לא משנה באופן משמעותי. למרות זאת, חשוב לשמור על היכולת להתמטר בתאריך מסוים באופן מדויק ולהתמטר חוזר ושוב.

חלופות נוספות ל-`chrono` ב-Rust כוללות חבילות כמו `time` ו`date`.

בעת המימוש, `chrono` נותנת לנו כמה פונקציות שאנחנו יכולים להשתמש בהן כדי להוסיף או להפחית ימים מהתאריך הנוכחי. 

## ראה גם
- [מסמך חומרת לימוד Rust](https://stevedonovan.github.io/rustifications/)
- [מדריך Rust ליישומים אינטרנטיים](https://stevedonovan.github.io/rustifications/)
- [מסמך יומן של Chrono](https://docs.rs/chrono/0.4.6/chrono/)