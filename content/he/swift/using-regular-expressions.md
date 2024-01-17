---
title:                "שימוש בביטויים רגילים"
html_title:           "Swift: שימוש בביטויים רגילים"
simple_title:         "שימוש בביטויים רגילים"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

# מה ולמה?

שימוש בביטויים רגולריים הוא דרך לחיפוש והתאמה של טקסט מתאים לתבנית מסוימת. זה מאפשר למתכנתים לבדוק ולעבד מידע בקלות ומהירות, ולאפשרות לפענח ולפענח מחרוזות בצורה יעילה.

# איך לעשות:

```Swift
let inputString = "ברוכים הבאים להרשמה לקורס פיתוח ויבסי"
let pattern = "הרשמה לקורס"
do {
    let regex = try NSRegularExpression(pattern: pattern)
    let matches = regex.matches(in: inputString, range: NSRange(inputString.startIndex..., in: inputString))
    print(matches.count) // Output: 1
} catch {
    print("Regex error: \(error.localizedDescription)")
}
```
בדוגמה זו, אנו מגדירים מחרוזת כניסה ותבנית לחיפוש. באמצעות טכניקות של דוגמה, אנו בודקים את הספירה של התבנית הנתונה מול המחרוזת המקורית.

# מקור עמוק:

עבור רוב המתכנתים, ביטויים רגולריים הינם כלי חיוני לפיתוח ותחזוקת תוכניות. כמו כן, הם נמצאים בשימוש נרחב באבטחת המידע, תכנות המחשב ואחרים. ישנן גם אלטרנטיבות לביטויים רגולריים, כגון פיתוח נאיבי ותכנות פיתון, אך הם לא מספקים את אותו רמת יעילות ופונקציונליות.

# ראו כן:

למאמר זה ישנן הרבה מקורות נוספים על ביטויים רגולריים בשפת Swift. כאן ניתן למצוא סרטוני לימוד, מדריכים ומאמרים כדי לעזור לך להתחיל ולשפר את כישוריך בשימוש בהם:

- [רכיבי String וסקנט](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [regex101](https://regex101.com/) - אתר חינמי לבדיקת ביטויים רגולריים עם תמיכה לשפת Swift
- [מדריך לביטויים רגולריים בSwift](https://medium.com/@abhimuralidharan/regular-expression-in-swift-2-0-3032271fcf20)