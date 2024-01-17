---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Rust: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים המתאימים לתבנית היא פעולה נפוצה בתכנות, שמטרתה למחוק תווים מתוך מחרוזת על פי תבנית ספציפית. תהליך זה נקרא גם "קיצוץ" או "הגבלה" ונעשה בהרבה שפות תכנות, כולל Rust. הסיבה לכך שתכניתנים מבצעים פעולה זו היא כי היא מאפשרת להם להעביר על מחרוזות וליצור מחרוזות חדשות בעזרת תיבת כלים קטנה ויעילה.

## איך לעשות זאת:
תהליך המחיקה של תווים המתאימים לתבנית ב-Rust פשוט ונעשה בעזרת הפונקציה ```remove_matches ()```. הנה כמה דוגמאות קוד למחיקת תווים המתאימים לתבנית "a" מתוך מחרוזות שונות:

```
fn main() {
    // מחיקת התו "a" מתוך המחרוזת
    let str = String::from("Hello world");
    let new_str = str.remove_matches("a");

    println!("{}", new_str); // פלט: "Hello world"

    // מחיקת התו "a" מתוך מחרוזת ריקה
    let empty_str = "".to_string();
    let new_empty_str = empty_str.remove_matches("a");

    println!("{}", new_empty_str); // פלט: ""

    // מחיקת התו "a" מתוך מחרוזת כוללת רק תא אחד
    let single_str = "a".to_string();
    let new_single_str = single_str.remove_matches("a");

    println!("{}", new_single_str); // פלט: ""
}
```

## כניסה מעמיקה:
אחת הפקודות המוכרות והנפוצות ביותר בעולם התכנות היא "מחק וגבול". בשפת תכנות חשובה זו, ישנם כמה אלטרנטיבות למחיקת תווים המתאימים לתבנית, כולל שימוש בפונקציות נוספות כמו "מחק וגבול של תווים" או "מחק תווים לפי תבנית". בנוסף, ידיעות הממניות לשפת תכנות יכולות לשחזר תא המכיל רק מחרוזת ריקה.

## ראו גם:
למידע נוסף על הפקודה ``` remove_matches () ``` והשתמשותה, יש להיכנס לדף התיעוד הרשמי של Rust בכתובת https://doc.rust-lang.org/std/string/struct.String.html#method.remove_matches. כמו כן, ניתן למצוא מידע נוסף ודוגמאות באתר הקהילתי DevDocs בכתובת https://devdocs.io/rust/std/string/struct.String.remove_matches.