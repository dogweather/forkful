---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת מספרים אקראיים היא פעולה שבה מחשב מחולל מספר שאין לנו צורך לדעת מהו מראש. מתכנתים משתמשים בה לצורך בחירה אקראית, טסטים אטלטיים, משחקים, וביטחון.

## איך לעשות:
בקוד של Rust, אפשר להשתמש בספרייה `rand` כדי ליצור מספרים אקראיים. להלן דוגמה:


```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let n: u32 = rng.gen();
    println!("Random Number: {}", n);
}
```
פלט:

```
Random Number: 36417
```

## צלילה עמוקה
ההיסטוריה של יצירת מספרים אקראיים במחשבים מתחילה בשנות ה-40 של המאה ה-20. ישנן שיטות נוספות כמו השימוש בגרעינים רדיואקטיביים או רעש תרמי, אבל רוב המקרים משתמשים באלגוריתמים מתמטיים. בשפת Rust, לדוגמה, האלגוריתם הוא איתני, דטרמיניסטי ויש לו סדר גודלו של כמה מיליארדים של מספרים אקראיים לא חוזרים.

## ראה גם:
* [Rust Book](https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html)
* [Rust API Documentation](https://doc.rust-lang.org/rand/rand/index.html)
* [Rust Cookbook](https://rust-lang-nursery.github.io/rust-cookbook/algorithms/randomness.html)