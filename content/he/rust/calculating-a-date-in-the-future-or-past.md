---
title:    "Rust: חישוב תאריך בעתיד או בעבר"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

בזמנים האחרונים, התכנות בשפת Rust הפך לפופולרי מאוד בקרב מתכנתים. שפת ראשיינו משלבת בין היכולות של שפות תכנות נמכרות נפוצות, כגון C ו־C++, ובין היבטים חדשים ומתקדמים יותר שנועדו להפוך את תהליך התכנות לפשוט יותר ואמין יותר. במאמר הזה נתמקד בחישוב תאריך בעבר או בעתיד באמצעות Rust.

## כיצד לעשות זאת

תחת הכותרת הזאת, יאפשר לקוראים ללמוד כיצד לכתוב קוד Rust שיכול לחשב תאריך בעבר או בעתיד לפי התאריך הנוכחי. במשלוחה הבאה, אנחנו נשתמש במודול "chrono" הכולל פונקציות לעיבוד תאריכים.

```Rust
use chrono::{Local, Duration};

fn main() {
    let today = Local::today();
    let past = today - Duration::days(7);
    let future = today + Duration::weeks(3);

    let formatted_past = past.format("%A, %B %e").to_string();
    let formatted_future = future.format("%A, %B %e").to_string();

    println!("In a week from now it will be {}, and three weeks from now it will be {}.", formatted_past, formatted_future);
}
```

בקוד הזה אנחנו משתמשים בפונקציות של Chrono כדי ליצור משתנה שמייצג את התאריך הנוכחי, ולאחר מכן להוסיף או להחסיר ימים או שבועות כדי לקבל תאריכים בעתיד או בעבר. סוף סוף, אנחנו מחלצים רק חלק מהמידע במשתנים פורמט תאריך ממש להדפסה.

החל מהתאריך הנוכחי, נראה איך התאריך שיוצא נראה כך:

```
In a week from now it will be Tuesday, February 23. three weeks from now it will be Tuesday, March 16.
```

## Deep Dive

בסעיף הזה נציג מידע נוסף על פונקציות חישוב תאריכים במודול Chrono שאינו מתפרט בפירוט ספציפי בתחתית כותרת "כיצד לעשות זאת". בנוסף, נוכל להעמיק טיפה יותר בנושא