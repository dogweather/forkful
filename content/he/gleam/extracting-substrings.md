---
title:                "Gleam: השכפלת תת-מחרוזות"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה

מי שמעוניין לשלוט בשפת תכנות Gleam ולהיות ביכולת למצוא ולהפעיל מחרוזות מסוימות בתוך מחרוזת גדולה, יכול להשתמש בפונקציות כדי לחלץ תתי-מחרוזת ולכן לאפשר עיבוד מסודר ויעיל של מידע.

## כיצד לעשות זאת

השימוש בפונקציות לחילוץ תתי-מחרוזת בשפת Gleam הוא פשוט ונוח. להלן דוגמאות לקוד עם פלט משתנה:

```Gleam
import gleam/string
str = "שפת תכנות Gleam מגיעה מיעד לכיף!😊"
substring = string.substring(str, 17, 27)
println(substring) // "מיעד לכיף"
```

ניתן גם להשתמש בפונקציות לחילוץ תתי-מחרוזת לעיבוד מסודר יותר של מחרוזות. לדוגמה, נבצע טיפול במספר טלפון ונחלץ את התחילת הקידומת והמספר הטלפון בנפרד:

```Gleam
import gleam/string
phone_number = "+972-555-123456"
prefix = string.substring(phone_number, 0, 4)
number = string.substring(phone_number, 5)
println(prefix) // "+972"
println(number) // "555-123456"
```

## Deep Dive

אם תרצו ללמוד עוד על התכנות בשפת Gleam וכיצד להשתמש בפונקציות לחילוץ תתי-מחרוזת, חשוב להבין שישנן פונקציות שונות עבור קבועים ומשתנים בפונקציה string.substring. לקבועים, הפונקציה תשלוט בתנאי ברור ולכן תעבוד בכל המצבים. לעומת זאת, במשתנים הפונקציה תעבוד רק כאשר המחרוזת מתאימה לתנאים המתאימים למשתנה.

אתם יכולים לקרוא עוד על עקרונות התכנות בשפת Gleam ועל פונקציות מתקדמות נוספות כדי לחלץ תתי-מחרוזת כאן: [פנקציות לחילוץ תתי-מחרוזת](https://gleam.run/documentation/std-lib-string#