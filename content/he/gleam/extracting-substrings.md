---
title:                "Gleam: חילוץ מחרוזות מתוך מחרוזת"
simple_title:         "חילוץ מחרוזות מתוך מחרוזת"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה
אחד לשתי משפטים המסביר למה מתבצעת תהליך של החלפת תווים תת-מחרוזת.

## כיצד לעשות זאת
להלן דוגמאות של קודים ופלט מוצלח לתת-מחרוזת, בתוך בלוקי קוד "```Gleam...```"

### דוגמא 1:
```Gleam
string = "Hello World"
substring = String.slice(string, 0, 5)
IO.println(substring)
```
פלט: "Hello"

### דוגמא 2:
```Gleam
numbers = ["1", "2", "3", "4", "5"]
range = List.slice(numbers, 2, 4)
IO.inspect(range)
```
פלט: ["3", "4"]

## צילום עמוק
תת-מחרוזת הוא פונקציה בשפת גלים שמאפשרת לך ליצור תת-מחרוזת חדשה מתוך מחרוזת קיימת. הפונקציה משתמשת בסמן הפלוס (+) להצביע על המקום של התו הסוף של התת-מחרוזת. למשל, אם נניח שנרצה ליצור תת-מחרוזת מן התו השני ועד התו הרביעי של מחרוזת, נשתמש בפונקציה String.slice ונציין את התווים שרוצים להכליל כמתרחקים בתוך סוגריים, למשל, `String.slice(string, 1, 3)`.

תת-מחרוזת נותנת לנו הנוכחות של קוד בלשונית שונה בתוך המחרוזת קיימת, ומאפשרת לנו לייצר מילות חדשות מתוך מחרוזות קיימות ולתמוך בפונקציות נוספות כמו String.replace ו- String.split.

## ראה גם
* [תיעוד רשמי של ספריית הגלים](https://gleam.run/documentation)
* [התחביר המקורי של גלים](https://github.com/gleam-lang/gleam/blob/main/core/src/String.gleam#L145-L152)