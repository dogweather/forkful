---
title:    "Gleam: המרת מחרוזת לאותיות קטנות"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## עבור מה

מדוע כדאי להמיר מחרוזת לאותיות קטנות בשפת גלים?

## איך לעשות זאת

הנה דוגמאות לקוד ולפלט בתוך בלוקי קוד "```Gleam ... ```".

קוד דוגמה:

```Gleam
import gleam/strings

let string = "This Is A Test"
let lower_case_string = strings.to_lower_case(string)

// פלט: "this is a test"
```

## חקירה מעמיקה

המרת מחרוזת לאותיות קטנות היא תהליך חשוב בתכנות גלים. המרת האותיות לאותיות קטנות מאפשרת לנו לבדוק תנאים ולעבוד עם נתונים בצורה נוחה יותר. בנוסף, זה עוזר להבין קוד נכתב על ידי אחרים בצורה טובה יותר.

## ראו גם

- [Strings Module in Gleam](https://gleam.run/standard-library/strings.html)
- [Gleam Documentation](https://gleam.run/docs/getting-started)