---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Swift: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

אנשים משתמשים במחיקת תווים התואמים לפעולות כמו ניקוי טקסטים או סינון תיבות מילון.

## איך לעשות זאת

עבור מחרוזות מסוימות, ניתן להשתמש בפקודת `replacingOccurrences(of:with:)` למחיקת התווים המתאימים לתבנית כלשהי. לדוגמה, נתון הטקסט הבא:
```Swift
let text = "היום הוא שישי"
```

נרצה להסיר את האות "ה" מתוך הטקסט. כדי לעשות זאת, נשתמש בפקודה הבאה:
```Swift
let newText = text.replacingOccurrences(of: "ה", with: "")
```

כתוצאה מכך, `newText` יהיה "יום ושישי".

## מעומקים

פקודת `replacingOccurrences(of:with:)` באמת מספקת את כל האפשרויות הנדרשות כדי למחוק תווים ממחרוזות, כולל תמיכה בתבניות רגולריות. לדוגמה, ניתן למחוק את כל התווים בכתובת המייל שמתחילים באות "h" עם הפקודה הבאה:
```Swift
let email = "example@domain.com"
let newEmail = email.replacingOccurrences(of: "h.+@", with: "", options: .regularExpression)
```

כאן, הפקודה מתאם לאתר תבנית כל תו המגיע אחרי האות "h" עד לתו ה"@", ומחליף את כל התווים האלה ברק מלחציות של מחרוזת ריקה. כתוצאה מכך, `newEmail` יהיה "@domain.com".

## ראה גם

- [המדריך הרשמי של Swift על פקודת `replacingOccurrences(of:with:)`](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID285)
- [הדרכות מתקדמות לאימון על תבניות רגולריות ב-Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)