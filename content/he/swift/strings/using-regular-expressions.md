---
title:                "שימוש בביטויים רגולריים"
aliases:
- /he/swift/using-regular-expressions.md
date:                  2024-02-03T19:18:53.584585-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בביטויים רגולריים"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
ביטויים רגולריים, או regex, הם רצפים של תווים היוצרים דפוס חיפוש, אשר לרוב משמשים למשימות תיאום או עיבוד מחרוזות. מתכנתים משתמשים בהם לכל דבר, החל מאימות נתונים ופירוק נתונים וכלה בהמרות, מה שהופך אותם לכלי בלתי נפרד במשימות עיבוד וניהול טקסט ברחבי שפות תכנות שונות, כולל Swift.

## איך לעשות:
התמיכה הטבעית של Swift ב regex משתמשת במחלקה `NSRegularExpression`, לצד המתודות range ו-replacement של מחלקת ה-String. להלן דוגמה לשימוש ב-regex כדי למצוא ולהדגיש כתובות דוא"ל בתוך בלוק טקסט:

```swift
import Foundation

let text = "צור קשר בכתובת support@example.com או feedback@example.org למידע נוסף."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("נמצא: \(text[range])")
        }
    } else {
        print("לא נמצאו התאמות.")
    }
} catch {
    print("שגיאת regex: \(error.localizedDescription)")
}

// פלט לדוגמה:
// נמצא: support@example.com
// נמצא: feedback@example.org
```

לסיטואציות מורכבות יותר או ממוקדות בנוחות, ניתן להשתמש בספריות צד שלישי כמו SwiftRegex, אשר מפשטות את התחביר ומרחיבות את האפשרויות. אף על פי שספריית הסטנדרט של Swift חזקה, כמה מפתחים מעדיפים את ספריות אלו בשל תחבירם התמציתי והתכונות הנוספות שהם מציעות. הנה איך תוכלו לבצע משימה דומה באמצעות ספרייה היפותטית של צד שלישי:

```swift
// בהנחה שקיימת ספרייה בשם SwiftRegex והיא מיובאת
let text = "פנה אלינו ב hello@world.com או בקר באתר שלנו."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // שיטה היפותטית המסופקת על ידי SwiftRegex
if emails.isEmpty {
    print("לא נמצאו כתובות דוא\"ל.")
} else {
    emails.forEach { email in
        print("נמצא: \(email)")
    }
}

// פלט היפותטי בהנחה שהשיטה `matches(for:)` קיימת ב-SwiftRegex:
// נמצא: hello@world.com
```

דוגמה זו ממחישה את השימוש בחבילה של ביטויים רגולריים של צד שלישי כדי לפשט את מציאת התאמות בתוך מחרוזת, בהנחה שקיימות שיטות נוחות כמו `matches(for:)`. חשוב להתייחס לתיעוד של הספרייה הספציפית של צד שלישי לקבלת תחביר מדויק וזמינות שיטה.
