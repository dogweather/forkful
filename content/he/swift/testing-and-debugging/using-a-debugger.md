---
date: 2024-01-26 04:11:15.215962-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05DE\u05E0\u05E4\
  \u05D4 \u05EA\u05E7\u05DC\u05D5\u05EA \u05D1-Xcode (\u05D4-IDE \u05E2\u05D1\u05D5\
  \u05E8 Swift), \u05D0\u05E4\u05E9\u05E8 \u05DC\u05D4\u05D2\u05D3\u05D9\u05E8 \u05E0\
  \u05E7\u05D5\u05D3\u05D5\u05EA \u05E2\u05E6\u05D9\u05E8\u05D4, \u05DC\u05D1\u05D3\
  \u05D5\u05E7 \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD, \u05D5\u05DC\u05E2\u05E7\u05D5\
  \u05D1 \u05D0\u05D7\u05E8\u05D9 \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD. \u05D4\
  \u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4."
lastmod: '2024-03-13T22:44:39.913267-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05DE\u05E0\
  \u05E4\u05D4 \u05EA\u05E7\u05DC\u05D5\u05EA \u05D1-Xcode (\u05D4-IDE \u05E2\u05D1\
  \u05D5\u05E8 Swift), \u05D0\u05E4\u05E9\u05E8 \u05DC\u05D4\u05D2\u05D3\u05D9\u05E8\
  \ \u05E0\u05E7\u05D5\u05D3\u05D5\u05EA \u05E2\u05E6\u05D9\u05E8\u05D4, \u05DC\u05D1\
  \u05D3\u05D5\u05E7 \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD, \u05D5\u05DC\u05E2\u05E7\
  \u05D5\u05D1 \u05D0\u05D7\u05E8\u05D9 \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  ."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D3\u05D9\u05D1\u05D0\u05D2\u05E8"
weight: 35
---

## איך לעשות זאת:
כדי להשתמש במנפה תקלות ב-Xcode (ה-IDE עבור Swift), אפשר להגדיר נקודות עצירה, לבדוק משתנים, ולעקוב אחרי ביטויים. הנה דוגמה:

```Swift
func findFactorial(of number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(of: number - 1)
}

let result = findFactorial(of: 5)
print(result)
```

כדי להגדיר נקודת עצירה, לחצו משמאל למספר השורה ב-Xcode, והריצו את התוכנית. כאשר היא פוגעת בנקודת העצירה, Xcode משהה את ההרצה. עכשיו אתם יכולים:

1. לבדוק ערכי משתנים.
2. לעבור קדימה (להריץ את השורה הבאה) או להיכנס פנימה (להיכנס לפונקציה) באמצעות פקדי המנפה תקלות.
3. להוסיף ביטויים ל'רשימת המעקב' כדי לעקוב אחרי שינויים במשתנים או קבועים מסוימים.

הנה מה שאתם עלולים לראות באזור הנפה תקלות:

```
(lldb) po number
5
(lldb) po result
120
```

## צלילה עמוקה:
מנפי תקלות היו חלק מנוף התכנות מאז שנות ה-40, והתפתחו ממערכות נקודת עצירה פשוטות לחוויות מורכבות המונעות על ידי ממשק משתמש. אופציות נוספות למעבר למנפה המובנה של Xcode כוללות כלים צד שלישי כמו LLDB (Low Level Debugger) ש-Xcode משתמש בו מאחורי הקלעים. ישנם אנשים שאף מנפים באמצעות הודעות `print()` (המכונה בחיבה "נפית תקלות האדם הקדמון"), אך זה פחות יעיל עבור פרויקטים גדולים או באגים מורכבים. כאשר אתם משתמשים במנפה תקלות, אתם מתמודדים עם שליטה בביצוע, פריסה רגעית בזמן ריצה, וניהול נתונים. הבנה עמוקה של העקרונות הללו תורמת רבות לנפית תקלות יעילה.

## ראו גם:
- [מדריך לנפית תקלות של Xcode מבית אפל](https://developer.apple.com/documentation/xcode/debugging/)
- [מדריך התחלה מהירה של LLDB](https://lldb.llvm.org/use/tutorial.html)
- [מדריך לנפית תקלות ב-Swift מבית Ray Wenderlich](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
