---
title:    "Swift: שימוש בביטויים רגילים"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה

כדאי להשתמש בביטויים רגילים כי הם מאפשרים מידע מפורט יותר וכן יכולים לחסוך זמן בכתיבת קוד.

## איך לעשות זאת

הן הכלים והתנאים שנדרשים כדי לשחזר את התבניות של התווים שהתאימו לביטויים רגילים. 

```Swift
let regex = try! NSRegularExpression(pattern: "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$", options: .caseInsensitive)
let email = "example@gmail.com"

if regex.firstMatch(in: email, options: [], range: NSRange(location: 0, length: email.count)) != nil {
  print("This is a valid email address.")
} else {
  print("This is not a valid email address.")
}
```

## חפירה עמוקה

הביטויים הרגילים יכולים לסייע בכתיבת קוד יעיל ומהיר יותר, כי הם מאפשרים לבצע התאמה וחילוץ של מידע מתוך טקסט לפי תבניות מסוימות. ניתן להשתמש בביטויים רגילים במגוון תחומים כגון עיבוד שפה טבעית, תיעודת קוד ובדיקות ודיקות משתנים.

## ראה גם

- [למדו להשתמש בביטויים רגילים ב-Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)
- [מדריך מפורט לביטויים רגילים בספריית התקנים של Apple](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/RegularExpressionMatching/Introduction.html)
- [ניתוח טקסט בעזרת ביטויים רגילים ב-Swift](https://www.raywenderlich.com/576-regular-expressions-tutorial-getting-started)