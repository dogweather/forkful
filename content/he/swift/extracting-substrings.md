---
title:                "חילוץ תת-מחרוזות"
aliases:
- he/swift/extracting-substrings.md
date:                  2024-01-20T17:46:33.697880-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות הוא תהליך שבו אתם לוקחים חתיכה מתוך מחרוזת קיימת. תכנתים עושים את זה כדי לקרוא, לעבד, או לנתח נתונים מסוימים במחרוזת.

## איך לעשות:
Swift מספקת כמה דרכים לחלץ תת-מחרוזות. הנה דוגמה:

```Swift
let fullString = "שלום, עולם!"
let startIndex = fullString.index(fullString.startIndex, offsetBy: 6)
let endIndex = fullString.index(fullString.startIndex, offsetBy: 10)
let substring = fullString[startIndex...endIndex]

print(substring) // תדפיס "עולם"
```

או באמצעות `Range`:

```Swift
let range = fullString.range(of: "עולם")!
let substring = fullString[range]

print(substring) // תדפיס "עולם"
```

## צלילה לעומק:
בעבר, בגרסאות קודמות של Swift, אנשים נעזרו במתודות NSString מ-Objective-C לחילוץ מחרוזות. עם הזמן, Swift הפכה ליעילה ומאפשרת עבודה ישירה עם מחרוזות באמצעות סינטקס מובנה שלה. השימוש בתת-מחרוזות ב-Swift מחזיר `Substring`, שהוא טיפוס שמשתמש בחלק מזיכרון המחרוזת המקורית ולכן הוא יעיל יותר מאשר ליצור מחרוזת חדשה.

## ראה גם:
- [דוקומנטציה הרשמית של Swift למחרוזות ותת-מחרוזות](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [מדריך ל-Ranges ב-Swift](https://www.avanderlee.com/swift/ranges-explained/)
