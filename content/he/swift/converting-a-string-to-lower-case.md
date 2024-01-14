---
title:                "Swift: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה:

אחת הפעולות הנפוצות בתכנות בשפת Swift היא המרת מחרוזת לאותיות קטנות (lower case). פעולה זו נעשית כדי לטפל במקרים שבהם שומרת הערך של המחרוזת אינה חשובה, וכאשר אנו רוצים להשוות אותה למחרוזת אחרת.

## איך לבצע:

בשפת Swift, ישנם שני דרכים פשוטות להמיר מחרוזת לאותיות קטנות:

```Swift
// דרך ראשונה: פעולת חיצוניות לפונקציית lowercased()
let str = "HELLO"
let lowerCaseStr = str.lowercased() // תוצאה: "hello"

// דרך שנייה: תכונת קובץ המחרוזות lowercased
let str1 = "WORLD"
let lowerCaseStr1 = str1.lowercased // תוצאה: "world"
```

## שכבות עמוקות:

כאשר אנו מבצעים המרה לאותיות קטנות בשפת Swift, התוכנית תמיד תכניס לפניך מחרוזת חדשה. למרבה המזל, תהליך ההמרה לאותיות קטנות הוא פשוט ובעל יעילות גבוהה במיוחד. אם תסתכלו בצורה חדשנית, תוכלו לראות כי כך מתבצעת ההמרה:

1. התוכנית מגדירה משתנה חדש, עם השם של המחרוזת המקורית.
2. התוכנית מגדירה משתנה חדש, עם השם של המחרוזת המקורית באותיות קטנות.
3. התוכנית מכניסה את המחרוזת המקורית לתוך המשתנה החדש.
4. התוכנית מכניסה את התוצאה לתוך המשתנה השני.

## ראה גם:

- [מדריך בשפת אנגלית על המרת מחרוזת לאותיות קטנות](https://www.hackingwithswift.com/example-code/strings/how-to-convert-a-string-to-lowercase)
- [מידע נוסף על פעולת חיצוניות לפונקציית lowercased()](https://developer.apple.com/documentation/swift/string/2427940-lowercased)
- [התחביר השלם של DFAS לא תקין](https://www8.cs.umu.se/kurser/TDBAfl/VT06/algorithms/BOOK/BOOK3/N