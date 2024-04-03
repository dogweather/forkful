---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:19.394325-07:00
description: "\u05D0\u05D9\u05DA \u05E2\u05D5\u05E9\u05D9\u05DD: \u05D1\u05E8\u05D0\
  \u05E1\u05D8, \u05D4\u05D8\u05D9\u05E4\u05D5\u05E1 `HashMap` \u05DE\u05D4\u05DE\u05D5\
  \u05D3\u05D5\u05DC `std::collections` \u05DE\u05E1\u05E4\u05E7 \u05D0\u05EA \u05E4\
  \u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05D4\u05DE\u05E2\
  \u05E8\u05DB\u05D9\u05DD \u05D4\u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\
  \u05D9\u05D9\u05DD. \u05D4\u05E0\u05D4 \u05D0\u05D9\u05DA \u05D0\u05E4\u05E9\u05E8\
  \ \u05DC\u05E2\u05D1\u05D5\u05D3 \u05D0\u05D9\u05EA\u05DD."
lastmod: '2024-03-13T22:44:38.975711-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05E8\u05D0\u05E1\u05D8, \u05D4\u05D8\u05D9\u05E4\u05D5\u05E1 `HashMap`\
  \ \u05DE\u05D4\u05DE\u05D5\u05D3\u05D5\u05DC `std::collections` \u05DE\u05E1\u05E4\
  \u05E7 \u05D0\u05EA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\
  \u05D5\u05EA \u05D4\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD \u05D4\u05D0\u05E1\u05D5\
  \u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD"
weight: 15
---

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
