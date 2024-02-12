---
title:                "קריאת פרמטרים משורת הפקודה"
aliases:
- he/swift/reading-command-line-arguments.md
date:                  2024-01-20T17:57:21.027041-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/reading-command-line-arguments.md"
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
