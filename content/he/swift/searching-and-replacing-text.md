---
title:                "חיפוש והחלפת טקסט"
date:                  2024-01-20T17:59:03.224716-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
פעולת חיפוש והחלפת טקסט היא בסיס לעבודה עם מחרוזות: אנחנו מחפשים תת-מחרוזת ומחליפים אותה במשהו אחר. זה נעשה לשם תיקון שגיאות, או מתן גמישות והתאמה אישית לתוכן.

## איך לעשות:
```Swift
let originalText = "זה טקסט לדוגמא. דוגמא טובה."
let searchText = "דוגמא"
let replacementText = "דוגמה"

let replacedText = originalText.replacingOccurrences(of: searchText, with: replacementText)
print(replacedText)  // "זה טקסט לדוגמה. דוגמה טובה."
```
שימו לב שהשימוש ב-`replacingOccurrences(of:with:)` לא משנה את המחרוזת המקורית, אלא יוצר מחרוזת חדשה.

## צלילה עמוקה
בשנים הראשונות של מחשבים, עיבוד טקסט היה משימה עקרית. עם קוד אסמבלי ושפות תכנות מוקדמות, החלפות נעשו בתהליכים קשים יותר. כיום ב-Swift, פונקציות מובנות כמו `replacingOccurrences(of:with:)` מקלות על התהליך. ישנם גם דרכים אחרות, כמו פעולת חתך ותפר (splice and dice) או שימוש בנוסחאות רגולריות (regular expressions) עבור החלפות מורכבות יותר. הבחירה במימוש תלויה במקרה השימוש שלכם.

## ראו גם
- [Documentation for Swift's String](https://developer.apple.com/documentation/swift/string)
- [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression) for more complex search and replace actions.
