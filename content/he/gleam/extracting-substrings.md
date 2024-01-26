---
title:                "חילוץ תת-מחרוזות"
date:                  2024-01-20T17:45:41.519275-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
לחלץ תת-מחרוזת פירושו לקבל חלק ממחרוזת קיימת. מתכנתים עושים זאת כדי לעבד טקסט ספציפי, לחתוך מידע לא נחוץ או למצוא ערכים חשובים בתוך מחרוזת.

## איך לעשות:
בגלים ניתן לחלץ תת-מחרוזת עם פונקציות סטנדרטיות. דוגמה:
```gleam
import gleam/string

pub fn main() {
  let greeting = "שלום עולם!"
  let world = string.slice(greeting, 6..10)
  world
}
```
פלט דוגמה:
```
"עולם"
```

## צלילה עמוקה
היכולת לחלץ תת-מחרוזות היא פונקציונליות בסיסית ברוב שפות התכנות וקיימת כבר עשרות שנים. בשפת גלים, שהיא פונקציונאלית ומתוכננת בביטחון, יש שימוש בפונקציות כמו `slice` לחלץ תת-מחרוזות. כלי זה נותן גמישות לעבוד עם גזרים מסוימים של טקסט. חלופות כוללות פונקציות כמו `split`, `starts_with`, ו-`ends_with` שגם הן יכולות לעבד מחרוזות בדרכים שונות.

## לראות גם
- תיעוד של Gleam על מחרוזות: https://hexdocs.pm/gleam_stdlib/gleam/string/
- מדריך למתכנתים: עבודה עם מחרוזות בGleam: https://gleam.run/book/tour/strings
- פונקציונליות מחרוזת נוספת במקור הפתוח של גלים: https://github.com/gleam-lang/stdlib
