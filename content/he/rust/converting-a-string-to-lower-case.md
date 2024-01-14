---
title:                "Rust: כותבים תכנית להמרת מחרוזת לאותיות קטנות"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

יחד עם שפת תכנות חדשה, מגיעות תפקודים חדשים וטרנדים חדשים. אחד הטרנדים הנפוצים בתחום התכנות הוא שימוש בקוד פשוט ופתרונות הסובבים קוד בכיף ויצירת קוד יעיל יותר. בכדי לקדם את התפתחות הדרישות הם הביאו את שפת "ראסט" לטכנולוגיות התכנות החדשות, ביחד עם חברות הטכנולוגיות הגדולות.

## איך לעשות זאת

הנה כמה דוגמאות לקוד פשוט ויישומי של איך להמיר מחרוזת למקרא נמוך במקרא פשוט:

```Rust
let name = String::from("JOSH");

let name_lower = name.to_lowercase();

println!("{}", name_lower);
```

תוצאה:

```JOSH```

בדוגמא השנייה, ניתן להמיר כל מחרוזת שנכתבת באנגלית למקרא נמוך באמצעות הפונקציה "to_lowercase". בתוך הפונקציה ניתן להכניס מחרוזת כדי להמיר אותו למקרא נמוך. ניתן לראות את הקוד המלא להמיר מחרוזת למקרא נמוך בדוגמא הבאה:

```Rust
use std::string::ToString;

fn main() {
    let word = "HELLO";

    let word_lower = word.to_lowercase();

    println!("{}", word_lower);
}
```

תוצאה:

```hello```

בדוגמא האחרונה, ניתן להמיר מחרוזת שמכילה אותיות מיוחדות, כגון תווים יפנים או כתבו הקירילי. תוצאה הקוד המלא להמיר מחרוזת למקרא נמוך מוצגת בחלק הבא:

```Rust
use std::string::ToString;

fn main() {
    let my_name = "דניאל";

    let my_name_lower = my_name.to_lowercase();

    println!("{}", my_name_lower);
}
```

תוצאה:

```דניאל```

ביותר!

## העמקה

מה קורה באמת כאשר משתמשים בפונקציה "to_lowercase" כדי להמיר מחרוזת למקרא נמוך? הבעיה היא שכשמשתמשים בפונקציה זו, היא לא יכולה לשנות את האותיות במקור