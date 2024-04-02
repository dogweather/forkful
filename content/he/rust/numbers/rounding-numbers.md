---
date: 2024-01-26 03:47:24.567133-07:00
description: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  \ \u05D0\u05D5\u05DE\u05E8 \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD \u05D0\u05D5\u05EA\
  \u05DD \u05DC\u05DE\u05E1\u05E4\u05E8 \u05D4\u05E9\u05DC\u05DD \u05D4\u05E7\u05E8\
  \u05D5\u05D1 \u05D1\u05D9\u05D5\u05EA\u05E8 \u05D0\u05D5 \u05DC\u05E9\u05D1\u05E8\
  \ \u05E2\u05DD \u05D3\u05D9\u05D5\u05E7 \u05DE\u05E1\u05D5\u05D9\u05DD. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E2\u05D2\u05DC\u05D9\u05DD \u05DE\u05E1\
  \u05E4\u05E8\u05D9\u05DD \u05DB\u05D3\u05D9 \u05DC\u05E4\u05E9\u05D8 \u05E2\u05E8\
  \u05DB\u05D9\u05DD \u05DC\u05E7\u05E8\u05D9\u05D0\u05D4 \u05E2\u05DC \u05D9\u05D3\
  \u05D9 \u05D1\u05E0\u05D9 \u05D0\u05D3\u05DD, \u05DC\u05E2\u05DE\u05D5\u05D3 \u05D1\
  \u05D3\u05E8\u05D9\u05E9\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.979027-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\
  \u05D5\u05DE\u05E8 \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD \u05D0\u05D5\u05EA\u05DD\
  \ \u05DC\u05DE\u05E1\u05E4\u05E8 \u05D4\u05E9\u05DC\u05DD \u05D4\u05E7\u05E8\u05D5\
  \u05D1 \u05D1\u05D9\u05D5\u05EA\u05E8 \u05D0\u05D5 \u05DC\u05E9\u05D1\u05E8 \u05E2\
  \u05DD \u05D3\u05D9\u05D5\u05E7 \u05DE\u05E1\u05D5\u05D9\u05DD. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05DE\u05E2\u05D2\u05DC\u05D9\u05DD \u05DE\u05E1\u05E4\
  \u05E8\u05D9\u05DD \u05DB\u05D3\u05D9 \u05DC\u05E4\u05E9\u05D8 \u05E2\u05E8\u05DB\
  \u05D9\u05DD \u05DC\u05E7\u05E8\u05D9\u05D0\u05D4 \u05E2\u05DC \u05D9\u05D3\u05D9\
  \ \u05D1\u05E0\u05D9 \u05D0\u05D3\u05DD, \u05DC\u05E2\u05DE\u05D5\u05D3 \u05D1\u05D3\
  \u05E8\u05D9\u05E9\u05D5\u05EA\u2026"
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
weight: 13
---

## מה ולמה?
עיגול מספרים אומר להתאים אותם למספר השלם הקרוב ביותר או לשבר עם דיוק מסוים. מתכנתים מעגלים מספרים כדי לפשט ערכים לקריאה על ידי בני אדם, לעמוד בדרישות מפרט, או להקטין את העומס החישובי בפעולות נקודה צפה.

## איך לעשות:
Rust הופך את העיגול לקל מאוד. בדקו את השיטות האלה עבור סוגי `f32` או `f64`:

```rust
fn main() {
    let num = 2.34567;

    // לעגל למספר השלם הקרוב ביותר
    let round = num.round();
    println!("Round: {}", round); // Round: 2

    // Floor - המספר השלם הגדול ביותר שאינו גדול מהמספר
    let floor = num.floor();
    println!("Floor: {}", floor); // Floor: 2

    // Ceil - המספר השלם הקטן ביותר שאינו קטן מהמספר
    let ceil = num.ceil();
    println!("Ceil: {}", ceil); // Ceil: 3

    // Truncate - חלקו השלם של המספר ללא הספרות העשרוניות
    let trunc = num.trunc();
    println!("Truncate: {}", trunc); // Truncate: 2

    // למספר השלם הקרוב ביותר של חזקה של עשר
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("מעוגל לשתי ספרות עשרוניות: {}", multiple_of_ten); // מעוגל לשתי ספרות עשרוניות: 2.35
}
```

## צלילה עמוקה
בהיסטוריה, עיגול היה קריטי להתמצאות עשרוניים אינסופיים או מספרים אי-רציונליים במרחבים דיגיטליים מוגבלים—חובה למחשבים עתיקים עם זיכרון מצומצם. חשבו על חשבונייה אבל פחות אומנותי, יותר מתמטיקה.

אלטרנטיבות לשיטות המובנות של Rust כוללות:
1. מקרו `format!` לעיצוב מחרוזת שמעגלת כברירת מחדל.
2. חבילות חיצוניות למשימות מתמטיות מורכבות, כמו חבילת `round` עם שליטה גרנולרית יותר.

מאחורי הקלעים, פעולות העיגול של Rust מתאימות לתקני IEEE—מונחי טכנולוגיה שמשמעותם "זה מעגל כמו שהמורה שלך למתמטיקה רוצה." פלוס, בגלל הייצוגים הבינאריים, קיימים מספרים שלא ניתן לעגל באופן מסורתי, כמו 0.1, בשל הייצוג האינסופי שלהם בבינארי.

## ראו גם
- מסמכי Rust על שיטות סוג פרימיטיבי: https://doc.rust-lang.org/std/primitive.f64.html
- התקן לחישוב צפה (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- חבילת "round" לעיגול מורכב יותר: https://crates.io/crates/round
