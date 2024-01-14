---
title:                "Gleam: מחיקת תווים התואמים דפוקלציה"
simple_title:         "מחיקת תווים התואמים דפוקלציה"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה:

מחיקת תווים שתואמים תבנית חשובה כאשר אנו כותבים קוד עם גלים בכדי לטפל בנתונים לפני שהם מעברים לשורות. מחיקת תווים לפי תבנית יכולה להיות שימושית למטרות מרובות, כגון תיקון תווים לא רצויים או שינוי של תבנית של טקסט ספציפי.

## איך לעשות זאת:

ניתן להשתמש בפונקציה המובנית delete_matching בשפת גלים כדי למחוק תווים לפי תבנית מסוימת. לדוגמה, אם אנחנו רוצים למחוק את התווים a, b ו-c מהמחרוזת "abcdefg", ניתן להשתמש בקוד הבא:

```Gleam
let str = "abcdefg"

let modified_str = delete_matching(str, [a,b,c])

IO.println(modified_str)

```

תוצאה:

```Gleam
defg
```
הפונקציה delete_matching מקבלת שני פרמטרים- מחרוזת ותבנית למחיקה. ניתן להעביר תבניות מסוגים שונים למחיקה, כגון תבניות של תווים ספציפיים, תבניות רגולריות ועוד.

## צלילה עמוקה:

מחיקת תווים לפי תבנית היא פעולה יסודית בתחביב הכתיבה עם גלים. ניתן לשנות את התבנית בזמן הריצה על ידי שימוש בפונקציות ידניות למחיקת תווים מתאימים. גם ניתן להשתמש בפונקציה delete_matching עם תבניות מסוגים מסובכים יותר כדי לטפל בכמויות גדולות של נתונים.

## ראה גם:

* [פונקציית delete_matching במדריך של גלים](https://gleam.run/documentation/function#delete_matching)
* [תכנות עם גלים למתחילים (בעברית)](https://blog.prototypr.io/%D7%AA%D7%9B%D7%A0%D7%95%D7%AA-%D7%A2%D7%9D-%D7%92%D7%9C%D7%99%D7%9D-%D7%9C%D7%9E%D7%AA%D7%97%D7%99%D7%9C