---
title:                "שרבוב מחרוזת"
aliases:
- he/swift/interpolating-a-string.md
date:                  2024-01-20T17:52:23.043758-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
שילוב טקסט במחרוזת (String interpolation) ב-Swift זה כשמכניסים משתנים וביטויים לתוך מחרוזת טקסט ישירות. זה מועיל למתכנתים לכתוב טקסטים דינמיים בקלות ובאופן קריא יותר.

## איך לעשות:
```swift
let name = "דני"
let age = 25
let greeting = "שלום, \(name)! אתה בן \(age) שנים."
print(greeting)
```
פלט דוגמה:
```
שלום, דני! אתה בן 25 שנים.
```

## עומק השקעה
ב-Swift, שילוב מחרוזות התחיל מהגרסה הראשונה והיה חלק מהיתרונות לעומת שפות קודמות. בשפות אחרות כמו C, למשל, צריך היה להשתמש בפונקציות כמו `sprintf` או לרכיב טקסט באמצעות צירוף מחרוזות מסורבל. שילוב מחרוזות ב-Swift מאפשר הכנסת ערכים וביטויים בצורה ישירה וקריאה, וכך משפר את כתיבת הקוד. השילוב נעשה בזמן ריצה של התוכנית, מה שמאפשר גמישות רבה ותגובה לשינויים בזמן אמת.

## ראה גם:
- [String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292) במדריך השפה של Swift.
- [String](https://developer.apple.com/documentation/swift/string) documentation ב-Apple Developer.
