---
title:    "Rust: השוואת שתי תאריכים"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

# למה: רק 1-2 משפטים המסבירים *למה* מישהו יתעסק להשוות שתי תאריכים.

שלום לכולם! בפוסט הזה אנחנו נדבר על השוואת שתי תאריכים בשפת Rust. כדי להבין טוב יותר את הנושא, נתחיל עם השאלה - למה מישהו בכלל צריך להשוות שתי תאריכים? התשובה פשוטה - כדי לבדוק אם שני אירועים קרו באותו יום, חודש וכו'. למשל, אם אתה רוצה לבדוק אם משהו התרחש לפני או אחרי מועד מסוים, או אם שתי אירועים קרו באותו יום.

## איך לעשות זאת: דוגמאות קוד ופלט דוגמה בתוך בלוקי קוד "```Rust...```"

בשפת Rust ישנם כמה דרכים שונות להשוות שתי תאריכים. לדוגמה, אפשר להשתמש בפונקציות המובנות "eq" (שוויון), "lt" (קטן מ) ו- "gt" (גדול מ) כדי לבדוק אם תאריך אחד שווה לתאריך אחר, קטן מתאריך אחר או גדול מתאריך אחר בהתאמה.

#### דוגמה 1:

```Rust
use chrono::{DateTime, Duration, Utc};

fn main() {
  let date1: DateTime<Utc> = Utc::now(); // יוצר תאריך נוכחי
  let date2: DateTime<Utc> = date1.checked_add_signed(Duration::days(7)).unwrap(); // מוריד שבוע מהתאריך הנוכחי
  let date3: DateTime<Utc> = date1.checked_sub_signed(Duration::days(7)).unwrap(); // מוסיף שבוע לתאריך הנוכחי

  println!("{}", date1.eq(&date2)); // הדפס שיאמר "לא לפני ולא אחרי"
  println!("{}", date1.lt(&date2)); // הדפס שיאמר "קטן"
  println!("{}", date2.gt(&date3)); // הדפס שיאמר "גדול"
}
```

פלט:
```
false
false
true
```

#### דוגמה 2:

```Rust
use chrono::{DateTime, Datelike, Utc};

fn main() {
  let date1: DateTime<Utc> = Utc::today(); // מחזיר את התאריך הידוע כיום
  let date2: DateTime<Utc> = Utc.ymd(2020, 1, 1).and_hms(0, 0, 0); // יוצר תאריך מסוים