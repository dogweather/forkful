---
date: 2024-01-20 17:57:21.027041-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Swift, \u05D0\
  \u05EA\u05D4 \u05D9\u05DB\u05D5\u05DC \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D0\u05E8\
  \u05D2\u05D5\u05DE\u05E0\u05D8\u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05E4\
  \u05E7\u05D5\u05D3\u05D4 \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D2\u05D9\u05E9\u05D4\
  \ \u05DC-`CommandLine.arguments`. \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E0\u05D9\
  \u05D7 \u05E9\u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8 \u05D4\u05E8\u05D0\u05E9\
  \u05D5\u05DF \u05D4\u05D5\u05D0 \u05D4\u05E0\u05EA\u05D9\u05D1 \u05DC\u05D1\u05D9\
  \u05E6\u05D5\u05E2\u05D9 \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA."
lastmod: '2024-03-13T22:44:39.931235-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-Swift, \u05D0\u05EA\u05D4 \u05D9\u05DB\u05D5\u05DC \u05DC\u05E7\u05E8\
  \u05D5\u05D0 \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\u05D9\u05DD \u05DE\u05E9\
  \u05D5\u05E8\u05EA \u05E4\u05E7\u05D5\u05D3\u05D4 \u05E2\u05DC \u05D9\u05D3\u05D9\
  \ \u05D2\u05D9\u05E9\u05D4 \u05DC-`CommandLine.arguments`."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
weight: 23
---

## איך לעשות:
ב-Swift, אתה יכול לקרוא ארגומנטים משורת פקודה על ידי גישה ל-`CommandLine.arguments`. ניתן להניח שארגומנט הראשון הוא הנתיב לביצועי התוכנית.

```Swift
// הדפס את כל הארגומנטים משורת הפקודה
for argument in CommandLine.arguments {
    print(argument)
}

// במקרה של קלט ספציפי, כמו קובץ לקריאה, מומלץ לוודא שהארגומנט קיים
if CommandLine.arguments.count > 1 {
    let fileName = CommandLine.arguments[1]
    print("שם הקובץ לעיבוד:", fileName)
} else {
    print("אנא הזן שם קובץ לעיבוד")
}
```

על ידי הרצת התוכנית מהטרמינל עם ארגומנטים, לדוגמה `swift myProgram.swift קובץ.txt`, תוכל לראות:

```
/Users/youruser/path/to/myProgram.swift
קובץ.txt
```

## צלילה לעומק
בעבר, לפני Swift, שפות כמו C פנו למערך ארגומנטים באופן ישיר דרך פרמטרים של `main()`. Swift תיקנה את זה לגישה נעימה יותר עם `CommandLine.arguments`. מעבר לזה, קיימים פריימוורקים וספריות כמו Swift Argument Parser שמספקים רמה גבוהה ועשירה של פרסור ארגומנטים. 
אגב, זכור שלא כל הארגומנטים בטוחים – הם יכולים להיות מניפולציה. הקפד לוודא ולאמת את הנתונים שהמשתמש מזין.

## ראה גם
- [Swift Argument Parser](https://github.com/apple/swift-argument-parser) – ספריה מבית Apple לניתוח משורת פקודה ב-Swift.
- [Swift Documentation](https://docs.swift.org/swift-book/ReferenceManual/AboutTheLanguageReference.html) – מדריך רשמי של שפת Swift.
- [Ray Wenderlich Command Line Tutorial](https://www.raywenderlich.com/511-command-line-programs-on-macos-tutorial) – הדרכה ליצירת תוכניות משורת פקודה ב-macOS.
