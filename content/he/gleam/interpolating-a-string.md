---
title:                "שרבוב מחרוזת"
date:                  2024-01-20T17:51:11.572244-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מילוי מחרוזת (string interpolation) הוא שיטה להכניס משתנים וביטויים לתוך מחרוזת. מתכנתים משתמשים בזה ליצירת מחרוזות דינמיות בקלות ובמהירות.

## איך לעשות:
ב-Gleam, מילוי מחרוזת נעשה באמצעות הפונקציה `string.concat` או פתרונות אחרים כמו צירופי מחרוזות. נתחיל עם `string.concat`:

```gleam
fn main() {
  let name = "עולם"
  let greeting = string.concat(["שלום ", name, "!"])
  io.println(greeting)
}
```

תוצאה: `שלום עולם!`

## עיון מעמיק
פעם, מילוי מחרוזות נעשה עם פונקציות כמו `sprintf` בשפות אחרות, אבל Gleam לוקחת מנגנון השראה משפות כמו Rust ו-Elm שנוטות להיות אקספליציטיות ובטוחות יותר. בימינו, `string.concat` ופונקציות דומות מאפשרות יצירת מחרוזת בצורה מאוד קריאה תוך שמירה על בטיחות סוג המשתנה. כלומר, מזהים עם סוגים ייחודיים לא יטעו אותנו בשגיאות כמו במערכות "loosely typed".

שיטה אחרת היא על ידי שימוש בפונקציה כמו `io.format`, שמעניקה יכולת ליצור מחרוזות מתוך תבניות מסוימות, דומות ברוחן ל-`sprintf`:

```gleam
fn main() {
  let name = "דוגמה"
  io.format("הנה ~p למילוי מחרוזת\n", [name])
}
```

תוצאה: `הנה "דוגמה" למילוי מחרוזת`

## ראה גם
- [Rust's string formatting](https://doc.rust-lang.org/std/fmt/)
- [Elm's string functions](https://package.elm-lang.org/packages/elm/core/latest/String)