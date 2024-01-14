---
title:                "Rust: הדפסת פלט לתיקון באגים"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

# למה

כתיבת הדפסת פלט לבעיות שיפור הדרגתיות של צוות פיתוח מאפשרת למפתחים למצוא בעיות ולכוון אותן באופן יעיל יותר. 

## כיצד לעשות זאת

```Rust
fn main() {
    let name = "David";
    println!("Hello, {}!", name);
}
```

כדי להדפיס הודעת debug ברוסט, יש להשתמש בפקודת `println!` ולהוסיף `#?` לסוגריים. כך ניתן להדפיס משתנים וערכים באופן דינמי ולנתח את הפלט בצורה ברורה יותר. 

## כניסה עמוקה

כאשר משתמשים בפקודת `println!` עם `#?`, היא משתמשת במראה הפנימי של כל ערך ומנתחת אותו באופן תחקירי. בנוסף, ניתן להשתמש בפקודת `dbg!` כדי להדפיס סטק טרייס. כל אלו מאפשרים למפתחים לזהות מקורות לבעיות ולהתמודד איתן. 

# ראה גם

- [מדריך לשפת תכנות Rust למתחילים](https://www.digitalocean.com/community/tutorials/how-to-install-rust-on-ubuntu-18-04)
- [תיעוד רשמי על פקודת `println!` ב־Rust](https://doc.rust-lang.org/std/macro.println.html)
- [הכרות עם פקודת `dbg!` של־Rust](https://towardsdatascience.com/debugging-rust-in-vs-code-7307b015f9ee)