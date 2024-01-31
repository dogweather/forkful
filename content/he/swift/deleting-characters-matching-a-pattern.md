---
title:                "מחיקת תווים התואמים לתבנית"
date:                  2024-01-20T17:43:44.819066-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים התואמים לתבנית היא פעולה שבה אנו מסירים קבוצה של תווים מתוך מחרוזת, המתאימה לדפוס מסוים. תכנותים נעשים את זה כדי לנקות קלט, להסיר תווים לא רצויים, או לעבד נתונים לפני המשך עיבוד.

## איך לעשות:
בSwift, תוכלו למחוק תווים התואמים לתבנית בעזרת `CharacterSet`, רגולר אקספרשנס, או מתודות מותאמות אישית. ככה זה עובד:

```Swift
var greeting = "שלום, עולם! 🌍👋"
let charactersToRemove = CharacterSet(charactersIn: "!,🌍👋")
let filteredGreeting = greeting.unicodeScalars.filter { !charactersToRemove.contains($0) }.string
print(filteredGreeting) // יודפס: "שלום עולם"
```
או עם רגולר אקספרשנס:
```Swift
var info = "שם: יוסי, גיל: 28, עיר: ירושלים"
let pattern = "[^א-ת ]" // תואם כל תו שאינו אות או רווח
if let regex = try? NSRegularExpression(pattern: pattern) {
    let range = NSRange(location: 0, length: info.utf16.count)
    info = regex.stringByReplacingMatches(in: info, range: range, withTemplate: "")
}
print(info) // יודפס: "שם יוסי גיל עיר ירושלים"
```

## עיון מעמיק
מחיקת תווים תואמי תבנית במחרוזות היא עניין ישן כמעט כמו תכנות עצמו. בעבר, שפות תכנות עשו זאת בצורות שונות, מפונקציות שורה אחת עד ליברריות מסובכות. כיום, בSwift, אפשר להשתמש במתודות שסטות האותיות (`CharacterSet`) למציאת תווים מוגבלים או ברגולר אקספרשנס לדפוסים מורכבים יותר.

מבחינת ביצועים, שקולו את כמות הנתונים שאתם מעבדים. `CharacterSet` מספקת ביצועים טובים לצורך הסרת תווים סטטיים, בעוד רגולר אקספרשנס יכולות להיות יעילות יותר לפעולות מורכבות אך יכולות גם להיות יקרות מבחינת משאבים.

לבסוף, שימוש ברגולר אקספרשנס דורש הבנה של הסינטקס שלהם וכיצד הם עובדים, כך שיש להשקיע זמן בלמידת הדפוסים וטסטינג התוצאות.

## ראו גם:
- [Apple Documentation on CharacterSet](https://developer.apple.com/documentation/foundation/characterset)
- [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swift String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Ray Wenderlich's Guide on Regular Expressions in Swift](https://www.raywenderlich.com/5765-regular-expressions-tutorial-getting-started)
