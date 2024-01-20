---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?

‏המרת משפט לאותיות קטנות, זו שיטה בה משנים את כל התווים במחרוזת לאותיות קטנות. מתכנתים המרים מחרוזות לאותיות קטנות כדי להפוך את ההשוואות לרגישות רוחב תווים.

## איך:

בעזרת הפונקציה `string.lowercase` שב-Gleam, ניתן להמיר מחרוזת לאותיות קטנות בקלות. להלן דוגמה:

```Gleam
import gleam/string

let my_string = "Hello World!"
let lower_case_string = string.lowercase(my_string)
```

דוגמא לפלט:

```Gleam
"hello world!"
```

## צלילה עמוקה

ההמרה לאותיות קטנות היא מקובלת בתכנות מאז תחילת ימי האסמבלר. בחלק מהשפות, כמו ב-Python ו-Java, ישנה פונקציה מובנית להמרת מחרוזת לאותיות קטנות. ב-Gleam ניתן להשתמש בפונקציה `string.lowercase`. כמה שפות מציעות אפשרויות אחרות, אבל הממשק המובנה של Gleam הוא הפשוט ביותר לשימוש.

על פי מימוש הפונקציה `string.lowercase` שב-Gleam, היא מבצעת את הפעולה על ידי הפעלת הפונקציה `Char.to_lower` על כל תו במחרוזת.

##ראה גם

1. [מסמך שפת Gleam](https://gleam.run/book/tour/strings.html)
3. [Erlang - string:lowercase/1](https://erlang.org/doc/man/string.html#lowercase-1)