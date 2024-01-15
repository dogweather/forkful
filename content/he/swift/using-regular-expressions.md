---
title:                "שימוש בביטויים רגולריים"
html_title:           "Swift: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

#למה
כדי להקל על עיבוד מחרוזות ותבניות מסוימות בקוד.

#כיצד להשתמש בביטויים רגילים עם סוויפט
##ביטויים רגילים בסוויפט שומרים על תבניות כתיבה מסוימות. לדוגמה, כדי למצוא מחרוזת שמתאימה לתבנית ספציפית, ניתן להשתמש בפונקציית `range(of:)` כך:

```Swift
let string = "Hello, world!"
let pattern = "[A-Z][a-z]+, [a-z]+!"
if let range = string.range(of: pattern, options: .regularExpression) {
  print(string[range])
}
```

פלט:

`Hello, world!`

כאן אנו מקופים את כל המחרוזת המתאימה לתבנית שלילתחילת באות גדולה ואחריה תווים באותיות קטנות, אפס או יותר מאותיות ריקות ולבסוף תו נקודתי.

#צלילות עמוקים
ישנן תבניות שונות שאפשר להשתמש בהן כדי לפרק ולזהות מחרוזות מסוימות, כגון רשימת מאפיינים ותווים תאמות. ניתן גם להשתמש בביטויים רגילים כדי לעזור בניתוח טקסט וכתיבת הקוד המתאים לפונקציות נתונות.

#ראה גם
- [דוקומנטציה רשמית לסוויפט](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [מדריך מעשי לביטויים רגילים עם סוויפט](https://medium.com/swift...regular-expressions-e2568476c1f6)
- [הכנסה משתמשת של סוויפט בביטויים רגילים](https://www.agnosticdev.com/blog-entry/using-regular-expressions-thin-swift)