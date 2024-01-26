---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:38:38.430531-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות היא פעולה שמשנה את כל תווי המחרוזת לאותיות קטנות. תכניתנים עושים זאת לנורמליזציה, להשוואה ולעיבוד טקסט.

## איך לעשות:
הנה דוגמה פשוטה בשפת Gleam שמראה כיצד להמיר מחרוזת לאותיות קטנות:

```gleam
import gleam/string

fn main() {
  let text = "Shalom, עולם!"
  let lowercased_text = string.lowercase(text)
  lowercased_text
}
```
תוצאה:
```
"shalom, עולם!"
```
שימו לב שהאותיות העבריות לא השתנו, מכיוון שהן כבר באותיות קטנות.

## עיון מעמיק:
במרוצת השנים, המרת מחרוזת לאותיות קטנות הפכה למרכיב מקובע ברוב השפות התכנות, כחלק ממניפולצית מחרוזת. במקרים מסוימים, ישנם עניינים של תרבות לשון ולקוח לאנגלית בשל הבדלים באותיות קטנות וגדולות. ישנן שיטות חלופיות כמו `to_uppercase` להמרת מחרוזת לאותיות גדולות, או שימוש ב-regex לפעולות מורכבות יותר. בתהליך המרה, שפות רבות משתמשות בטבלאות פנימיות כדי למפות כל אות להמרתה התואמת.

## ראה גם:
- דוקומנטציית המודול `string` ב-Gleam: https://hexdocs.pm/gleam_stdlib/gleam/string/
- מאמר על נורמליזציה של מחרוזות: https://en.wikipedia.org/wiki/Unicode_equivalence
- מדריך לביטויים רגולריים (Regex): https://www.regular-expressions.info/
