---
title:                "שרשור מחרוזות"
aliases:
- he/swift/concatenating-strings.md
date:                  2024-01-20T17:36:11.622563-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרשור מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?
קיטון מחרוזות זה פשוט לדביק ביחד שניים או יותר טקסטים ליצירת מחרוזת אחת. תכניתנים עושים את זה כדי לבנות משפטים, מסרים, או פורמטים דינמיים בקוד.

## איך לעשות:
```Swift
let firstName = "יונתן"
let lastName = "כהן"
let fullName = firstName + " " + lastName // שימוש באופרטור חיבור
print(fullName) // פלט: יונתן כהן

// עם interpolation של מחרוזות
let greeting = "שלום, שמי \(firstName) \(lastName)!"
print(greeting) // פלט: שלום, שמי יונתן כהן!

// דרך שלישית עם append
var message = "מר"
message += " "
message += fullName
print(message) // פלט: מר יונתן כהן
```

## צלילה עמוקה
בעבר, קיטון מחרוזות היה לעיתים קרובות פעולה לא יעילה. זה בגלל שהיה צריך ליצור אובייקטים חדשים בזיכרון בכל פעם שמחברים מחרוזות. בשפות מודרניות כמו Swift, קיטון מחרוזות הוא בדרך כלל יעיל יותר, עם מיטובי זיכרון וביצועים שנעשו על ידי מהדר השפה.

כלי חלופיים לקיטון כוללים שימוש במחלקות כמו `NSMutableString` ב-Objective-C, או `StringBuilder` ב-Java, שמועילים במקרים של עיבוד מחרוזות כבד.

ב-Swift, המיטוב נעשה 'תחת המכסה' כך שהתכניתנים יכולים לקטון מחרוזות בצורה אינטואיטיבית, מבלי לחשוש מתוצאות לא יעילות.

## גם כן ראה
- [String Interpolation in Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift Programming from Scratch](https://www.weheartswift.com/swift-programming-scratch-100-exercises/)
- [Apple's Swift String Documentation](https://developer.apple.com/documentation/swift/string)
