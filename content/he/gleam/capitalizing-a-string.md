---
title:                "הפיכת מחרוזת לאותיות גדולות"
html_title:           "Gleam: הפיכת מחרוזת לאותיות גדולות"
simple_title:         "הפיכת מחרוזת לאותיות גדולות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizing Strings ב-Gleam: כיצד להפוך את האות הראשונה במחרוזת לאות גדולה

## מה זה & למה?

דאגה למחרוזת עם אות ראשונה גדולה היא משימה שמתקיימת לעיתים קרובות בתכנות. זה משמש בעיקר להצגת תכנים באופן יותר מקצועי ומסודר.

## איך לעשות:

בגלים ניתן להשתמש בפונקציה `string.capitalize`. הנה דוגמה:

```Gleam
import gleam/string

let my_string = "שלום עולם"
string.capitalize(my_string)
// יוצג: "שלום עולם"
```

כמו שאתה רואה, הפונקציה המעטה תחזיר את אותה המחרוזת. זה כי Gleam עדיין לא תמיכה בגבולות מילה עבור תווים שאינם ASCII, אז האות הראשונה שהיא תו שלא מ-ASCII לא הופכת לאות גדולה.

## Deep Dive

ככל שהגרסאות של Gleam מתקדמות, אנו מצפים לראות תמיכה מחודשת בשפות שאינן אנגליות, כולל תמיכה טובה יותר במילים שאינן-ASCII. כרגע, ניתן להוסיף אות גדולה באמצעות שילוב מוחלט. זו הכרחית לשפת תכנות שדואגת שהמשתמש יהיה מסוגל להבין את התכולה שנוצרת מכאן.

## See Also

אני ממליץ לקרוא גם עוד יותר נושאים כאלה בשפת התכנות Gleam בדף הבית של Gleam [הקישור מפנה כאן](https://gleam.run).