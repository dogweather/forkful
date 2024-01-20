---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
חפיפת Debug בראסט היא תהליך שבו מתכנת מציג מידע של התהליך בהוצאה לפועל, ומשמש כעזרה מכריעה בהבנת הקוד ופתרון בעיות. זה הכלי האידיאלי לדיאגנוסטיקה ותיקון בעיות בקוד שלך.

## איך לעשות:
עזרתו של סימן ```{:?}``` מאפשרת למשתמש להדפיס את הנתונים בפלט Debug. 
קוד בדוגמה:

```Rust
let data = [1, 2, 3, 4, 5];
println!("{:?}", data);
```

הפלט יהיה:
```
[1, 2, 3, 4, 5]
``` 
בראסט גם מאפשרת לנו להדפיס את שלבי הביצוע של פונקציות באמצעות לוגים. 
קוד נוסף:

```Rust
debug!("this is a debug message for {}", "function x");
info!("this is an info message for {}", "function y");
warn!("this is a warning message for {}", "function z");
error!("this is an error message for {}", "function a");
```

ניווט לאפשרות:
```
cargo run -- --verbosity debug
```

## Deep Dive       
1.בהקשר ההיסטורי, המאפיין הראשוני של Debug היה בתקן Algol-60 בשנת 1960. מאז, המאפיין התפתחות והתהדר עם תמיכה מתקדמת ושלמות למערכות בנייה מורכבות ודינמיות.

2. בראסט מחמיאה לגישת ה-Explicit over implicit עם Debug. שימוש במאפיין ```{:?}``` במקום ```{}``` הופך להיות ברור מאיפה המידע מגיע. 

3. למעשה, מאפיין Debug הוא פונקצית מאקרו אשר מפענחת את הנתונים ומציגה אותם בצורת טקסט עם מידע מהקוד המקורי.

## ראה גם: 
[Rust's official documentation on the Debug trait](https://doc.rust-lang.org/std/fmt/#formatting-traits)  
[The Rust Programming book chapter about debugging](https://doc.rust-lang.org/book/ch09-00-error-handling.html)  
[Discussion about implementing formatting traits in Rust](https://users.rust-lang.org/t/why-do-we-need-to-manually-implement-display-and-debug-traits/30399)