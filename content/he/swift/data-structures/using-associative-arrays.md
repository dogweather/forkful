---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:14:01.687390-07:00
description: "\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\
  \u05D8\u05D9\u05D1\u05D9\u05D9\u05DD, \u05D4\u05D9\u05D3\u05D5\u05E2\u05D9\u05DD\
  \ \u05DB\u05DE\u05D9\u05DC\u05D5\u05E0\u05D9\u05DD \u05D1-Swift, \u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05D9\u05DD \u05DC\u05DA \u05DC\u05D0\u05D7\u05E1\u05DF \u05D5\u05DC\
  \u05E0\u05D4\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DB\u05D6\u05D5\u05D2\
  \u05D5\u05EA \u05DE\u05E4\u05EA\u05D7-\u05E2\u05E8\u05DA. \u05EA\u05D5\u05DB\u05E0\
  \u05D9\u05EA\u05E0\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\
  \u05D4\u05DD \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E8\u05D2\u05DF \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D1\u05D9\u05E2\u05D9\u05DC\u05D5\u05EA, \u05D5\u05DC\u05D4\
  \u05E7\u05DC \u05E2\u05DC\u2026"
lastmod: '2024-03-13T22:44:39.894002-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\
  \u05D8\u05D9\u05D1\u05D9\u05D9\u05DD, \u05D4\u05D9\u05D3\u05D5\u05E2\u05D9\u05DD\
  \ \u05DB\u05DE\u05D9\u05DC\u05D5\u05E0\u05D9\u05DD \u05D1-Swift, \u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05D9\u05DD \u05DC\u05DA \u05DC\u05D0\u05D7\u05E1\u05DF \u05D5\u05DC\
  \u05E0\u05D4\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DB\u05D6\u05D5\u05D2\
  \u05D5\u05EA \u05DE\u05E4\u05EA\u05D7-\u05E2\u05E8\u05DA."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD"
weight: 15
---

## איך לעשות:
Swift מקל על העבודה עם מערכים אסוציאטיביים. הנה איך אפשר להגדיר, להוסיף, להסיר ולגשת לפריטים במילון של Swift:

```Swift
// הכרזה על מילון
var fruitColors: [String: String] = ["Apple": "Red", "Banana": "Yellow"]

// הוספת פריט חדש
fruitColors["Grape"] = "Purple"

// גישה לערך באמצעות המפתח
if let appleColor = fruitColors["Apple"] {
    print("Apple is \(appleColor).")  // פלט: Apple is Red.
} else {
    print("Color not found.")
}

// הסרת פריט
fruitColors["Banana"] = nil  // זה יסיר את "Banana" מהמילון

// חילוץ על פריטים
for (fruit, color) in fruitColors {
    print("\(fruit) is \(color).")
    // פלט:
    // Apple is Red.
    // Grape is Purple.
}
```

מילונים הם מאוד גמישים, מאפשרים לך לתפעל ולגשת לנתונים באופן דינמי. הטבע הלא ממוין שלהם אינו משפיע על מהירות אחזור הנתונים, שזה יתרון ניכר כשעובדים עם ערכות נתונים גדולות.

## צלילה עמוקה
היישום של מילונים ב-Swift כמערך אסוציאטיבי מגיע מהיכולת העוצמתית שלהם למפות מפתחות ייחודיים לערכים. בהיסטוריה, שפות תכנות מימשו את הרעיון הזה תחת שמות שונים כמו טבלאות גיבוב או מפות, המרמזים על הפונקציונליות שלהם ליצור "מפה" בין מפתחות לערכים.

ב-Swift, המילונים מאופטימים לביצועים, באמצעות שימוש במפתחות הניתנים לגיבוב לאחזור נתונים ביעילות. זה אומר שסוג ה`Key` במילון `[Key: Value]` חייב לעמוד בפרוטוקול `Hashable`, שזה נכון לרוב סוגי הסטנדרט של Swift כמו `Int`, `String`, ו-`Double`.

דבר אחד שכדאי לשקול הוא שלמרות שמילונים מעולים להתאמות בין זוגות נתונים, הם חסרים סדר. אם אתה צריך לשמור על סדר האלמנטים, ייתכן שתחפש אלטרנטיבות כמו `Array` עבור רצף של אלמנטים מסודרים או מבני נתונים מותאמים אישית שמשלבים תכונות של שניהם, מערכים ומילונים.

כמו כן, ראוי לציין ש-Swift מתפתחת באופן רציף, וכך גם התמיכה והאופטימיזציות של מילונים. לכן, חשוב להישאר מעודכנים עם התיעוד האחרון של Swift כדי לנצל את המרב מהמילונים, ולהבטיח שאתה משתמש בתרגולים היעילים והעדכניים ביותר.
