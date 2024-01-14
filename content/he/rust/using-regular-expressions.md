---
title:                "Rust: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה

למה לעסוק בשימוש בביטויים רגולריים? בקיצור, בהינתן מחרוזת טקסט, ביטוי רגולרי מאפשר לנו לחפש ולהתאים תבניות מסוימות של מילים או תווים, מה שיכול להיות שימושי כשאנחנו מסתכלים על טקסטים מורכבים ורבי-תבניתיים.

## איך לעשות זאת

ראשית, כדאי להתחיל מהתאם את הפייתון שלנו כדי שיתאים לגרסא המתאימה של ובכיוון זה הוא Rust. אם כבר יש לכם ידע בכתיבת קוד ב-Rust, את הביטויים הרגולריים יהיה יחסית קל לימוד.

מצד שני, אם אין לכם ידע בכתיבת קוד, אפשר גם להשתמש בכלים נוספים שמאפשרים לנו ליצור ולבדוק תבניות של ביטויים רגולריים כמו regexr.com או regextester.com.

אם תבחרו לכתוב קוד באופן ידני, נדרשת יכולת כתיבת קוד חזקה כדי לשלב את הביטויים הרגולריים בתוך קוד Rust שלנו. הנה דוגמה ליצירת ביטוי רגולרי פשוט שמזהה כמה תווים מסוימים במחרוזת ומדפיס אותם:

```Rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\w{3}").unwrap();
    let text = "hello world";
    for mat in re.find_iter(text) {
        println!("{}", mat.as_str());
    }
}
```

פלט:
```console
hel
llo
wor
```

כדי להתאים תבניות מורכבות יותר, יכולים להשתמש בתווי מטא כמו \d למספרים או \w לאותיות או תווים, וכן להשתמש בכמה סמכויות כמו * (אפס או יותר) או + (אחד או יותר). לדוגמה:

```Rust
let re = Regex::new(r"\d{3}-\d{3}-\d{4}").unwrap();
let text = "My phone number is 555-123-4567";
println!("{}", re.replace_all(text, "XXX-XXX-XXXX"));
```

פלט:
```console
My phone number is XXX-XXX-XXXX 
```