---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט היא פעולה שבה מידע מחולקת מקובץ טקסט למשתנים בתוכנית. מתכנתים עושים את זה כדי לנהל דינמית מידע שמשתנה, לדוגמה, הגדרות, רישומים ממנות יומית, או פרמטרים שנקלטים ממשתמשים.

## איך לעשות:
להלן דוגמה למנגנון קריאת קובץ טקסט ב-Gleam:

```Gleam
import gleam/iod.file.{read}
import gleam/bit_builder.{append, to_string}
import gleam/otp

Case TestIod {
  output("hello.txt") |> bit_builder.append("שלום עולם!") |> bit_builder.to_string
}
```
הקובץ "hello.txt" מכיל עכשיו את המחרוזת "שלום עולם!".

## עומק השקיעה
1. **הקשר ההיסטורי:** גישה זו לעיבוד קבצים מקורה בעבודה עם מערכות קבצים שמבוססות טקסט של שפות תכנות קלאסיות משנות ה-70 וה-80.
2. **אלטרנטיבות:** ישנן שפות תכנות אחרות שמציעות שיטות שונות לקריאת קובצים. גם ב-Gleam ניתן להשתמש באפשרויות נוספות, כמו לדוגמה עיבוד בזרימה.
3. **פרטי אמצעי קיום:** Gleam מימשת את הפונקציות על ידי שליחת בקשת I/O למערכת ההפעלה.

## ראו גם
הקישורים הבאים מציגים מידע נוסף ובהם ניתן לבחון את נושא קריאת קובץ בעומק רב יותר:

1. [Gleam I/O library](https://hexdocs.pm/gleam_stdlib/gleam/iod/file/#read)
2. [File I/O in other languages](https://en.wikipedia.org/wiki/File_Input/output)
3. [Stream processing in Gleam](https://github.com/gleam-lang/stdlib)