---
date: 2024-01-20 17:53:53.565328-07:00
description: "How to: / \u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\
  -Swift, \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1-`print()` \u05D6\u05D4 \u05E4\u05E9\
  \u05D5\u05D8."
lastmod: '2024-04-05T22:37:48.355142-06:00'
model: gpt-4-1106-preview
summary: "/ \u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Swift, \u05E9\
  \u05D9\u05DE\u05D5\u05E9 \u05D1-`print()` \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8."
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD"
weight: 33
---

## How to: / איך לעשות:
ב-Swift, שימוש ב-`print()` זה פשוט:
```Swift
let greeting = "שלום עולם"
print(greeting)
// הפלט: שלום עולם
```

לדוגמא עם משתנים מרובים וריכוז:
```Swift
let name = "דני"
let age = 30
print("שם: \(name), גיל: \(age)")
// הפלט: שם: דני, גיל: 30
```

ניתן גם להשתמש ב-`debugPrint()` לפלט מפורט יותר:
```Swift
struct Person {
    var name: String
    var age: Int
}

let meir = Person(name: "מאיר", age: 25)
debugPrint(meir) 
// הפלט: Person(name: "מאיר", age: 25)
```

## Deep Dive / נטילה עמוקה:
הדפסת פלט ב-Swift אינה חדשנית - היא אבן יסוד בתכנות מאז ימי שפות תכנות מוקדמות כמו C. הפונקציה `print()` ב-Swift יכולה לטפל במספר רב של סוגי נתונים באופן דינאמי, שיפור על פונקציות תכנות C כמו `printf()`.

קיימות גם שיטות נוספות, כמו מערכת הלוגים המובנית של Apple `os_log`, או להשתמש בקונסולת המפתחים או בקובץ יומן.

בצד היישום, השיטה `print()` כוללת כברירת מחדל סיומת שורה `\n`. ניתן לשלוט בזה על ידי העברת תיקונים לפונקציה, כגון `terminator` ו-`separator`.

## See Also / ראה גם:
1. תיעוד רשמי של Swift על `print()` - [דפים רשמיים](https://developer.apple.com/documentation/swift/1541053-print)
2. מדריך ל-`os_log` מבית Apple - [דפים רשמיים](https://developer.apple.com/documentation/os/logging)
3. רעיונות לניפוי באגים אפקטיביים ב-Swift - [Ray Wenderlich](https://www.raywenderlich.com/3030-ios-debugging-in-xcode)
