---
title:                "Rust: הדפסת פלט תיקונים"
simple_title:         "הדפסת פלט תיקונים"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

/* Shalom lechol ha-chaverim ha-mishtalechim shba-Rust! Beshvil artikl zeh, tchinam latzet al hegemara shel debug output ba-safek yom-me-hod. L'mat ha-artikl metsuyar la-or hav`mek zot ha-mahanehhut shel be`ur ha-tushia ha-ze.b'artikl ha-zeh, taqvim lhiyot medabrim al mah hu debug output, miltei mlka hayom ba-sibilme hah-d. At lmedim lhibonot pigurim lhibonot sheleshe reddu yt yoplakat ruhst.HaAnnual Rust Conference
## Why

למה לעשות דבר מגעיל כמו להדפיס את תוכן האתחלה של התכנית אליכם? אולי יש איזו עניין אסטרונמי אחר?

אם תמיד נתנסה להבין כיצד התכנית פועלת ואיפה הבעיות. אבל לדיי קשה לספר תארכוהיים עם הדפסת רגילה או לבזת לב ׃;רחוב עם דפוס.

## How To

אם אתה מעוניין להדפיס את קוד Rust שלך, יש לנו כמה טכניקות.

```Rust
fn main() {
    println!("זהו דוגמא של הדפסה פשוטה");
    
    let number = 15;
    println!("מספר שלם : {}", number);
    
    let bool_value = true;
    println!("ערך בוליאני : {}", bool_value);
}
```

כאן אנו משתמשים בפקודת `println!` כדי להדפיס את הערכים, עם הכנתקיק המתאים לסוג הנתון. הנה כמה דוגמאות נוספות כדי לסייע לך להבין איך להשתמש בהדפסה פונקצילה :

```Rust
fn main() {
    let name = "יעקב";
    let age = 30;
    
    println!("שם : {}, גיל : {}", name, age);
}

// פלט:
// שם : יעקב, גיל : 30
```

כמובן שאתה יכול להשתמש גם בפקודות אחרות להדפסה, כמו `eprintln!` להדפסת שגיאות או `format!` ליצירת מחרוזת מעוצבת עם הערכים הרלוונטיים.

## Deep Dive

הדפסת תוכן האתחלה היא כלי חשוב בעבודת השיפור הרבה. כמו כן, זה יכול לעזור לנו למצוא בעיות ותקלות בקוד שלנו. בקוד Rust ישנם כמה אפש