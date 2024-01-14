---
title:    "Gleam: להגדיר מתח בכתיב עילי"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה

 למה לחשוב כי הכתיבה באותיות גדולות של מחרוזת מסוימת חשובה: קל יותר להבדיל בין מילה או בין אמצעי חיצוני ( למשל, מנעול חלון בסרגל הכלים במחשב האישי) ממילה אחרת.

## כיצד לעשות זאת

הדרך הטובה ביותר לכתוב את התכנית שלך תלויה במטרות התוכנית כמו כן בסיבת החיפוש הספציפי בתכנות. הנה כמה דוגמאות בשפת גלים:
```Gleam
// Change to uppercase
let string = "hello"
let uppercase = String.to_upper(string)
// Output: "HELLO"

// Change to lowercase
let lowercase = String.to_lower(string)
// Output: "hello"

// Capitalize first letter
let first_capitalized = String.capitalize(string)
// Output: "Hello"

// Capitalize first letter of each word
let all_capitalized = String.title_case(string)
// Output: "Hello"
```

## בירור עמוק

כאשר מפעילים את הפונקציה לשינוי האותיות לאותיות גדולות, נדרשת פעולה של המרת נתונים מסוג string ל string, ולאחר מכן החזרת הנתונים מחדש למקום המקורי. ניתן גם להשתמש בספריה חיצונית כדי לשנות את גודל האותיות ישירות מהטקסט. ישנן גם אופציות נוספות לשינוי אותיות, כמו שינוי מתאים בהקוון או שינוי לטבעת קווים.

## ראה גם

למידע נוסף על שינוי אותיות בשפת גלים, ניתן לקרוא את המדריך הרשמי: https://gleam.run/documentation/#string-transformations

לפרטים נוספים על שפת גלים בשפת עברית, ניתן לגלות את הפרויקט המקומי כאן: https://github.com/Gleam-Lang/gleam-lang.github.io/blob/master/README.md

והנה כמה מאמרים נוספים על לימוד גלים:
https://medium.com/@omerxx/how-i-learned-gleam-programming-language-be3e13d083c5
https://www.aopen.co.il/prod2019/intro-gleam.pdf