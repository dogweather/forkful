---
date: 2024-01-26 01:16:48.447329-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05D3\u05DE\u05D9\u05D9\u05E0\u05D5 \u05DE\u05E9\u05D9\u05DE\u05D4: \u05DC\u05D7\
  \u05E9\u05D1 \u05D0\u05EA \u05D4\u05DE\u05DE\u05D5\u05E6\u05E2 \u05E9\u05DC \u05DE\
  \u05E2\u05E8\u05DA. \u05D1\u05DC\u05D9 \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\
  \u05EA, \u05D4\u05D9\u05D9\u05EA\u05DD \u05DE\u05DB\u05E0\u05D9\u05E1\u05D9\u05DD\
  \ \u05D4\u05DB\u05DC \u05DC-main. \u05E2\u05DD \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\
  \u05D5\u05EA, \u05D4\u05D9\u05D9\u05EA\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05DB\
  \u05DB\u05D4."
lastmod: '2024-04-05T22:37:48.359117-06:00'
model: gpt-4-0125-preview
summary: "\u05D3\u05DE\u05D9\u05D9\u05E0\u05D5 \u05DE\u05E9\u05D9\u05DE\u05D4: \u05DC\
  \u05D7\u05E9\u05D1 \u05D0\u05EA \u05D4\u05DE\u05DE\u05D5\u05E6\u05E2 \u05E9\u05DC\
  \ \u05DE\u05E2\u05E8\u05DA. \u05D1\u05DC\u05D9 \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\
  \u05D5\u05EA, \u05D4\u05D9\u05D9\u05EA\u05DD \u05DE\u05DB\u05E0\u05D9\u05E1\u05D9\
  \u05DD \u05D4\u05DB\u05DC \u05DC-main. \u05E2\u05DD \u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D5\u05EA, \u05D4\u05D9\u05D9\u05EA\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05DB\u05DB\u05D4."
title: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA"
weight: 18
---

## איך לעשות זאת:
דמיינו משימה: לחשב את הממוצע של מערך. בלי פונקציות, הייתם מכניסים הכל ל-main. עם פונקציות, הייתם עושים ככה:

```swift
func calculateAverage(of numbers: [Double]) -> Double {
    let sum = numbers.reduce(0, +)
    return numbers.isEmpty ? 0 : sum / Double(numbers.count)
}

// שימוש
let scores = [92.5, 88.75, 99.0, 70.5]
let averageScore = calculateAverage(of: scores)
print("הממוצע הוא \(averageScore)")
```

הפלט היה יכול להיות:
```
הממוצע הוא 87.6875
```

## לעומק
היסטורית, ככל שתכנות התעצם, פונקציות הפכו לאבן פינה לניהול סיבוכיות. אלטרנטיבות כוללות קוד אינליין והעתק הדבק של קוד (ספגטי קוד) – שנחשבות כיום לשיטה לא רצויה. בשפת Swift, פונקציות הן אזרחיות ממעלה ראשונה; ניתן להקצות אותן למשתנים, להעביר אותן כארגומנטים, ולהחזיר אותן מפונקציות אחרות, מה שהופך את הקוד למודולרי וגמיש יותר.

מבחינת יישום, עצבו את הפונקציות שלכם לעשות דבר אחד ולעשותו טוב. שאפו לפונקציות עם מטרה ברורה ושם שמשקף אותה. שימו לב למספר הפרמטרים - אם יש רבים מדי, סביר להניח שאתם עושים יותר מדי. טיפול בשגיאות? שקלו להשתמש בפונקציות שמטילות ולטפל בבעיות בחן. זכרו: Swift מתמקדת בקריאות ובקלות תחזוקה.

## ראו גם
- [מדריך לשפת התכנות Swift - פונקציות](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [מדריך סגנון Swift של Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
- [שיפוץ: שיפור עיצוב קוד קיים של מרטין פאולר](https://martinfowler.com/books/refactoring.html)
