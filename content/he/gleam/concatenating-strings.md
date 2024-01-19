---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## ?מה ולמה
שרשרת (concatenating) מחרוזות היא דרך לחבר מחרוזות של טקסט יחד. מתכנתים עשויים לבצע זאת כאשר הם יוצרים או מתאימים טקסט להקרנה, שמירה או שיתוף למשתמשים או יישומים אחרים.

## איך ל:
שימוש בפקודה concat לשרשור מחרוזות ב-Gleam:

```Gleam
let message1 = "שלום, "
let message2 = "עולם!"
let greeting = message1 |> String.concat(message2)
```

הפלט של הקוד הזה יהיה: "שלום, עולם!".

## הצצה מעמיקה
- קונטקסט היסטורי: במחשבים הראשונים, היכולת לשרשר מחרוזות הייתה קומפקטית ובמקור הוכנסה לשפות כמו בשל. למדנו לאהוב את היכולת הזו והיא ממשיכה להתפשט בכל השפות - ברובם - בצורה מקננונית מאז.
- חלופות: ברוב השפות, ניתן לשרשר מחרוזות באמצעות בנאי טקסט או באמצעות פונקציות לשרשור ישיר. ב-Gleam זה בהחלט אפשרי באמצעות '+' או 'concat'.
- פרטים על איך זה מבוצע: בתוך המנוע, רשימת מחרוזת שמשרשרת מחרוזת לרשימה אחרת נוצרת. את המחרוזות מאחדים לװמחרוזת הסופית ״.

## ראה גם
- [מדריך Official Gleam](https://gleam.run/tour/#?part=basics)
- [מדריך למחרוזות ב-Gleam](https://gleam.run/book/tour/strings.html) 
- [מבוא לשפת Gleam](https://gleam.run/introduction/)