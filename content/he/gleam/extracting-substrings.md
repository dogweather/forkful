---
title:                "חילוץ מחרוזות ממחשבים"
html_title:           "Gleam: חילוץ מחרוזות ממחשבים"
simple_title:         "חילוץ מחרוזות ממחשבים"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה
ֿֿעם המכניס [Substring](https://en.wikipedia.org/wiki/Substring) נטמע בתוך מחרוזות מבתנווית, לפעמים יש לנו צורך לחלץ רכיבים ספציפיים מתוך המחרוזת כדי לעבוד עליהם בנפרד או לשנות אותם. לדוגמא, אם יש לנו מחרוזת המכילה שם מלא כמו "John Smith", ואנחנו רוצים להציג רק את שם המשפחה "Smith", אנו נצטרך לחלץ את הנתונים המתאימים. בכתב זה, נתאר כיצד להשתמש בשפת תכנות גלאם כדי לחלץ רכיבים ספציפיים מתוך מחרוזת.

## איך לעשות זאת 
כדי לחלץ substring בגלאם, ניתן להשתמש בפונקציה `substring` ולמענה על שלושה פרמטרים: המחרוזת המקורית, האינדקס של התחילת הsubstring והאינדקס של הסיום של הsubstring. לדוגמא, אם נניח שיש לנו את המחרוזת "Hello World" ונרצה להחליף את המילה "World" ב"Universe", נוכל לעשות זאת בעזרת הקוד הבא בשפת גלאם:

```Gleam
let original_string = "Hello World"
let new_string = substring(original_string, 6, 11)
```

התוצאה יהיה "Universe". כדי לחלץ את השם המשפחה מתוך המחרוזת "John Smith", נוכל להשתמש בקוד הבא:

```Gleam
let full_name = "John Smith"
let last_name = substring(full_name, 5, 9)
```

התוצאה תהיה "Smith".

## Deep Dive
עכשיו שאנו יודעים איך להשתמש בפונקציה `substring`, נבין כיצד היא פועלת במימוש. פונקצית `substring` מקבלת שלושה פרמטרים: המחרוזת המקורית, האינדקס של התחילת הsubstring והאינדקס של הסיום של הsubstring. היא מייצרת מחרוזת חדשה על ידי שרשור רכיבי המחרוזת המקורית שממוקמים בין האינדקסים שצוינו. גם כאשר האינדקסים