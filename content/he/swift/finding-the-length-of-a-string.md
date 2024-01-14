---
title:                "Swift: מציאת אורך מחרוזת"
simple_title:         "מציאת אורך מחרוזת"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה

בניגוד לשפות תכנות אחרות, ג'וי סוויפט אינה מכילה פונקציה מובנית לקביעת אורך של מחרוזת. על מנת לבדוק את אורך המחרוזת, יש לכתוב קוד מיוחד שיחשב כמה תווים נמצאים במחרוזת.

## איך לעשות זאת

כדי למצוא את אורך המחרוזת ב-Joy Swift, ניתן להשתמש בפעולה count על משתנה מחרוזת. הרי דוגמה פשוטה שמציגה את השימוש בפעולה זו:

```Swift
let myString = "זהו דוגמה למחרוזת."
let stringLength = myString.count
print(stringLength) // Output: 20
```

כאן אנו משתמשים במשתנה בשם myString שמכיל מחרוזת ובפעולת count עליו כדי לבדוק את אורך המחרוזת. התוצאה שמתקבלת היא מספר התווים הכוללים במחרוזת, ובמקרה זה היא 20. ניתן גם להשתמש בפעולה זו על מחרוזות ריקות או מחרוזות עם רווחים.

## עומק התהליך

כמו שציינו מקודם, ניתן להשתמש בפעולה count גם על מחרוזות ריקות. במקרה זה היא תחזיר את התוצאה 0. כמו כן, יש לזכור שהפעולה תחשב גם תווים בעברית ובאותיות גדולות. למשל:

```Swift
let myString = "אבגד"
let stringLength = myString.count
print(stringLength) // Output: 4
```

בדוגמה זו, אנו משתמשים במחרוזת המכילה את האותיות העבריות אבגד. למרבה המזל, הפעולה count יחשב גם את התווים בעברית כתווים אחדים ותחזיר תוצאה תקינה של 4.

## ראו גם

- [פעולות על מופעים במסגרת מחרוזת](https://swiftdoc.org/v2.2/language/guide/strings_and_characters/#manipulating-chars)
- [תכונת count של טיפוס String](https://developer.apple.com/documentation/swift/string/2894664-count)