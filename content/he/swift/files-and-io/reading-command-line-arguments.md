---
date: 2024-01-20 17:57:21.027041-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05E4\u05E7\u05D5\u05D3\u05D4 \u05DE\
  \u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA\
  \ \u05DC\u05E7\u05D1\u05DC \u05E7\u05DC\u05D8 \u05D3\u05D9\u05E0\u05DE\u05D9 \u05DE\
  \u05D4\u05DE\u05E9\u05EA\u05DE\u05E9. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\
  \u05D0\u05D9\u05DD \u05D0\u05EA \u05E4\u05E2\u05D5\u05DC\u05EA \u05D4\u05EA\u05D5\
  \u05DB\u05E0\u05D9\u05EA \u05DC\u05D3\u05E8\u05D9\u05E9\u05D5\u05EA \u05D4\u05DE\
  \u05E9\u05EA\u05DE\u05E9 \u05D1\u05D6\u05DE\u05DF \u05E8\u05D9\u05E6\u05D4."
lastmod: '2024-02-25T18:49:38.172584-07:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05E4\u05E7\u05D5\u05D3\u05D4 \u05DE\
  \u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA\
  \ \u05DC\u05E7\u05D1\u05DC \u05E7\u05DC\u05D8 \u05D3\u05D9\u05E0\u05DE\u05D9 \u05DE\
  \u05D4\u05DE\u05E9\u05EA\u05DE\u05E9. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\
  \u05D0\u05D9\u05DD \u05D0\u05EA \u05E4\u05E2\u05D5\u05DC\u05EA \u05D4\u05EA\u05D5\
  \u05DB\u05E0\u05D9\u05EA \u05DC\u05D3\u05E8\u05D9\u05E9\u05D5\u05EA \u05D4\u05DE\
  \u05E9\u05EA\u05DE\u05E9 \u05D1\u05D6\u05DE\u05DF \u05E8\u05D9\u05E6\u05D4."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת פקודה מאפשרת לתוכניות לקבל קלט דינמי מהמשתמש. תכנתים עושים זאת כדי להתאים את פעולת התוכנית לדרישות המשתמש בזמן ריצה.

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
