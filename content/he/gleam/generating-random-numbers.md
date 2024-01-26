---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:49:18.766091-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
גרילת מספרים אקראיים היא פעולה שבה אנו יוצרים מספרים שאינם צפויים מראש. תכנותים משתמשים בזה למגוון סיבות כמו בדיקות, משחקים ובחירות אקראיות.

## איך לעשות:
כדי להשיג מספרים אקראיים ב-Gleam, נשתמש בפונקציה מתוך החבילה `gleam_stdlib`.
```gleam
import gleam/stdlib
import gleam/random

fn main() {
  let seed = random.default_seed() 
  let (num, _) = random.int(seed, 1, 10) 
  num
}
```
פלט דוגמא: `7` (הערה: מספר אקראי, יכול להיות שונה בכל הרצה)

## עיון נוסף
בעבר, גרילת מספרים אקראיים הייתה עניין פחות טריוויאלי. מכונות אינן טובות ביצירת אקראיות טהורה, כך שפיתחו אלגוריתמים לזריעת אקראיות פסאודו-אקראית. בבחירת מספרים אקראיים חשוב לשקול את שיטת הזריעה וכמה "אקראי" המספר אכן צריך להיות. לגלים יש את החבילה `gleam_stdlib` שכוללת פונקציות עזר לגרילת מספרים בצורה פשוטה וגם למודלים מתקדמים יותר של אקראיות.

## ראה גם
- [Wikipedia article on Pseudo-random number generation](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
