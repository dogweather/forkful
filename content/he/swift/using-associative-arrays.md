---
title:                "שימוש במערכים אסוציאטיביים"
date:                  2024-01-30T19:14:01.687390-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במערכים אסוציאטיביים"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

מערכים אסוציאטיביים, הידועים כמילונים ב-Swift, מאפשרים לך לאחסן ולנהל נתונים כזוגות מפתח-ערך. תוכניתנים משתמשים בהם כדי לארגן נתונים ביעילות, ולהקל על גישה ושינוי ערכים בהתבסס על המפתחות הייחודיים שלהם.

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