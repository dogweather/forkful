---
title:    "Rust: שימוש בביטויים רגילים"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה

כתיבת קוד היא כמו פאזל חשיבתי. אך לפעמים יש דברים שמאתגרים אותנו ככל שננסה לפתורם. כאלה הם כלים כמו רגקספים, אשר נועדו למצוא ולהתאים למבנה מסוים בטקסט. אם אתה מחפש דרך חדשה ומעניינת לטפל בעניינים כאלו בתוכנות שלך, אז רגקסים ב־Rust עשויים להיות הפתרון המתאים עבורך.

## איך להשתמש

כדי להתחיל להשתמש ברגקסים ב־Rust, נדרשת לנו ספרייה בשם "regex". אחר כך, נוכל להשתמש בפונקציות וכלים רבים שזמינים לנו מתוך הספרייה הזו.

```Rust
use regex::Regex;

fn main() {
    let pattern = Regex::new(r"hello").unwrap();
    let text = "hello world";
    println!("Found a match: {}", pattern.is_match(text));
}

// Output:
// Found a match: true
```

הקוד הזה מציג לנו את המילה "hello" בתור הגדרת התבנית שאנחנו מחפשים. נציג טקסט קצר בכדי לראות האם נמצא תואם לתבנית שלנו. במקרה הזה, נמצא תואם והקוד ידפיס לנו "Found a match: true".

הנה עוד דוגמה של כיצד נוכל להשתמש ברגקסים כדי למצוא ולהחליף מחרוזות בתוך טקסט:

```Rust
use regex::Regex;

fn main(){
    let pattern = Regex::new("cookie").unwrap();
    let text = "I really love cookies!";
    let replaced_text = pattern.replace_all(text, "cake");
    println!("Replaced text: {}", replaced_text);
}

// Output:
// Replaced text: I really love cake!
```

בדוגמה זו, אנחנו מחפשים את המחרוזת "cookie" ומחליפים אותה במחרוזת "cake". זהו רק דוגמה פשוטה לכיצד נוכל להשתמש ברגקסים כדי לטפל בטקסט ולשנות אותו לפי הצורך שלנו.

## חקירה מעמיקה

המאפיינים של רגקסים הם מאוד חשובים ללמוד כדי לה