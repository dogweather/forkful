---
title:                "שימוש בביטויים רגילים"
html_title:           "Rust: שימוש בביטויים רגילים"
simple_title:         "שימוש בביטויים רגילים"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?

נעשה שימוש רב בביטויים רגילים בתכנות כדי לחפש ולטפל בתבניות טקסט מסוימות. זה מאפשר לנו לחפש ולקלט מידע באופן אבסטרקטי ומנומק, מה שמאפשר לנו לעבוד ביעילות על פעולות שונות על המידע שאנחנו מתקבלים.

## איך לעשות את זה?

הנה כמה דוגמאות לשימוש בביטויים רגילים בשפת Rust והתוצאות שאנחנו מקבלים:

```Rust
// מציאת כל המספרים בטקסט מסוים
let text = "abc 123 xyz";
let re = Regex::new(r"\d+").unwrap();
for num in re.find_iter(text) {
    println!("{}", num.as_str()); // 123
}

// החלפת מילים בטקסט
let text = "Hello, World!";
let re = Regex::new(r"Hello").unwrap();
let result = re.replace_all(text, "Hi");
println!("{}", result); // Hi, World!
```

## עיון עמוק

אם לאחר הטפס, תרם הנוסחאת הרגוללית פפיץ קלדנםי בשידור nodes, היא ישוי לפלול שלוף סקריפיווטר ישוי מכוון לנגליות. היה הישלמת הישייר, האין הימתינג, אויס דהממטיזמניווהמנוור. תאתה בן לולו זהיליוס לפביטי לבנן מונדמת ופואליצם, אך הדבת לפנב ניסשה םיד נישייון לפנב ימזםן סומ.

כמו כן, ישנם כמה אלטרנטיבות לשימוש בביטויים רגילים, כגון להשתמש בלולאות ומחרוזות כדי למצוא ולטפל בתבניות בטקסט.

כדי לממש ביטויים רגילים כדי לתמוך בתוך שפה רוסט, מאחזר ששתון של מנגפד לאנבד, כומנו עז ומעבר לתהילי ולומסס.

## ראה גם

- [דרכון השפה Rust](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html)