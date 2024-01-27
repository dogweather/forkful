---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
בשימוש רגולרי בביטויים תכניתיים, אנו מחפשים ומניפולים טקסט בהתאם לתבניות מורכבות. זה כלי חזק לממצא ואימות נתונים שכיח בפרויקטים תכנותיים.

## איך לעשות:
```Swift
import Foundation

let testString = "אהלן, החשבון שלך הוא 123-45-6789."
let pattern = "\\b\\d{3}-\\d{2}-\\d{4}\\b"

do {
    let regex = try NSRegularExpression(pattern: pattern)
    let matches = regex.matches(in: testString, range: NSRange(testString.startIndex..., in: testString))

    if let match = matches.first {
        let range = Range(match.range, in: testString)!
        print(testString[range]) // הדפיסו "123-45-6789"
    }
} catch {
    print("שגיאה ביצירת ביטוי רגולרי: \(error.localizedDescription)")
}
```

## נים תוך:
ביטויים רגולריים הם לא חידוש - הם התפתחו בשנות ה-50 והם בשימוש נרחב בכל היבט של מחשוב. ישנן חלופות כמו חיפוש בינארי או פרסינג של סינטקס, אבל לעיתים רק ביטוי רגולרי יעשה את העבודה. Swift משתמש ב-`NSRegularExpression`, אשר מגיע מ-Objective-C ומתאים לכל טקסט Unicode, כולל עברית.

## ראו גם:
- [Apple's NSRegularExpression Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Ray Wenderlich's Regular Expressions Tutorial](https://www.raywenderlich.com/5765-regular-expressions-tutorial-getting-started)
- [RegexOne: Learn Regular Expressions with simple, interactive exercises.](https://regexone.com/)
