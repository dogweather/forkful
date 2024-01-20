---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?

המרת מחרוזת לאותיות קטנות היא תהליך שבו כל אות גדולה במחרוזת מומרת לאות קטנה. תכנתים עשויים לעשות זאת כדי לבצע השוואות לא תלויות-הרגש בין מחרוזות.

## כיצד לבצע:

ב-Elixir, ניתן להמיר מחרוזת לאותיות קטנות באמצעות כמה דרכים. אחת מהן היא להשתמש בפונקציה downcase במודול String.
```Elixir
string = "HELLO"
lower_string = String.downcase(string)
IO.puts(lower_string)
```
פלט
```
hello
```
## הצצה לעומק:

**הקשר היסטורי**: בעבר, מחשבים היו מתייחסים לאותיות קטנות וגדולות כאילו הן שונות לחלוטין. אך, בעולם של הכתוב, מקרה (או האם זה גדול או קטן) פשוט אינו משנה, לכן הפעולה של המרת מחרוזת לאותיות קטנות הפכה לחיונית. 

**חלופות**: ייתכן שתרצו לבדוק אם כל מחרוזת כבר נמצאת בתוך אותיות קטנות לפני שאתם ממירים אותה, תוכלו לעשות זאת באמצעות פונקציה `String.equivalent?/2`, אז תשתמשו ב- `String.downcase/1` אם נדרש.

**פרטי המימוש**: ב-Elixir, מינימיזציה נמשכת באופן אוטומטי לשימוש של UTF-8. זה מגנה על מחרוזות של הדמיה של תווים או שמות מקומיים מפך.

## נסו גם:

אתה יכול הסתכל יותר מעמיק, אנחנו ממליצים לך לבדוק את [דוקומנטציית Elixir](https://hexdocs.pm/elixir/String.html#downcase/1) ואת [מדריך Unicode ב-Elixir](https://elixir-lang.org/blog/2020/10/06/unicode-and-elixir-a-love-story/) כדי להבין מה קורה מאחורי הקלעים כאשר אתה ממיר אותיות גדולות לאותיות קטנות.