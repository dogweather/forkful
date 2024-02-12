---
title:                "שימוש במערכים אסוציאטיביים"
aliases:
- /he/rust/using-associative-arrays.md
date:                  2024-01-30T19:13:19.394325-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במערכים אסוציאטיביים"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

מערכים אסוציאטיביים, או מה שראסטיינים קוראים להם "מפות גיבוב", הם אוספים ששומרים נתונים בזוגות מפתח-ערך. מתכנתים משתמשים בהם לחיפוש נתונים מהיר, מה שמאפשר מניפולציה יעילה של נתונים בהתבסס על מפתחות ייחודיים.

## איך עושים:

בראסט, הטיפוס `HashMap` מהמודול `std::collections` מספק את פונקציונליות המערכים האסוציאטיביים. הנה איך אפשר לעבוד איתם:

```Rust
use std::collections::HashMap;

fn main() {
    // יצירת HashMap חדש
    let mut scores = HashMap::new();

    // הוספת ערכים
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // גישה לערכים
    let team_name = String::from("Blue");
    if let Some(score) = scores.get(&team_name) {
        println!("ניקוד לקבוצת בלו: {}", score); // פלט: ניקוד לקבוצת בלו: 10
    }

    // עדכון ערך
    scores.entry(String::from("Blue")).and_modify(|e| *e += 5);

    // חיפוש מעבר על זוגות מפתח-ערך
    for (key, value) in &scores {
        println!("{}: {}", key, value); // פלט: Blue: 15, Yellow: 50
    }
}
```

## ניתוח עמוק

ה`HashMap` בראסט משתמש בפונקציה של גיבוב למיפוי מפתחות לערכים, מה שמאפשר חיפוש נתונים מהיר. עם זאת, יעילות זו באה עם מחיר: מפות גיבוב אינן שומרות על סדר האלמנטים שלהן. זה בניגוד ליישומי מערכים אסוציאטיביים אחרים, כמו אלה בפייתון (`dict`) או ברובי, שבגרסאות האחרונות מתחזקים את סדר ההכנסה כתכונה. למקרים שבהם סדר זוגות המפתח-ערך חשוב, מפתחי ראסט יכולים לשקול להשתמש ב`BTreeMap` מהמודול `std::collections`, ששומר על סדר אך עשוי להציע הכנסה וחיפוש איטיים יותר לעומת `HashMap`. בסופו של דבר, הבחירה בין `HashMap` ל`BTreeMap` תלויה בדרישות הספציפיות בנוגע לסדר וביצועים.
