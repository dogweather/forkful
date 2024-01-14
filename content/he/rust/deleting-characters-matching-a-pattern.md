---
title:                "Rust: מחיקת תווים התואמים לתבנית"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה 

מחיקת תווים המתאימים לתבנית יכולה להיות כלי מועיל בתכנות בשפת ראסט. זה עשוי להיות על מנת לנקות טקסט תקליטור, לשנות סדר של תווים, או למצוא ולהסיר תווים חסרים בטקסט. 

## איך לעשות 

תחילה, נצטרך לייבא את המודול המתאים לפעולת מחיקת תווים מתאימים לתבנית. נעשה זאת על ידי הוספת השורה הבאה לראש הקובץ: 

```Rust 
use regex::Regex; 
```

לאחר מכן, נגדיר את התבנית של התווים שנרצה למחוק. לדוגמה, אם נרצה למחוק את כל התווים הגדולים באותיות, נכתוב: 

```Rust 
let pattern = Regex::new("[A-Z]"); 
``` 

לבסוף, נשתמש בפקודה `replace_all` כדי למחוק את התווים המתאימים לתבנית: 

```Rust 
let result = pattern.replace_all("Hello World", ""); 
``` 

הנה כיצד הקוד השלם יראה עם כל השורות יחד: 

```Rust 
use regex::Regex; 

fn main() {
    let pattern = Regex::new("[A-Z]"); 
    let result = pattern.replace_all("Hello World", ""); 
    println!("{}", result); 
}
``` 

והנה הפלט של קוד הפעולה: 

``` 
ello orld 
``` 

## יצירת תקנה חכמית (עומס) 

כאשר משתמשים במודול Regex בפרויקט גדול יותר, ייתכן שנתקלו בבעיות עומס מתיחות. במצבים כאלה, כדאי ליצור תקנה חכמה במקום לאתחל את התבנית מחדש בכל פעם שאנו רוצים לבצע מחיקה מתאימה. תחת התקנה החכמה, נגדיר את התבנית כמשתנה גלובלי ונפעיל את הפעולה `replace_all` על המחרוזת הרצויה: 

```Rust 
use regex::Regex; 

fn main() {
    lazy_static! {
        static ref PATTERN: Regex = Regex::new("[A-Z]").unwrap();
    }

    let result = PATTERN.replace_all("Hello World", "");
    println!("{}", result);
}
``` 

כעת, התבנית תימצא רק פעם אחת לכל