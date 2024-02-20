---
date: 2024-01-20 17:43:04.612503-07:00
description: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\
  \u05DE\u05EA\u05D0\u05D9\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA\
  \ \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05E1\u05D9\
  \u05E0\u05D5\u05E0\u05D9\u05DD \u05DE\u05D5\u05D7\u05DC\u05D9\u05DD \u05E2\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E1\
  \u05D9\u05E8 \u05EA\u05D5\u05D5\u05D9\u05DD \u05DC\u05D0 \u05E8\u05E6\u05D5\u05D9\
  \u05D9\u05DD. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E7\u05D5\u05EA \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD, \u05DC\u05D4\u05E4\u05D5\u05DA \u05E4\u05D5\u05E8\u05DE\
  \u05D8 \u05DC\u05E7\u05D5\u05D4\u05E8\u05E0\u05D8\u05D9 \u05D0\u05D5\u2026"
lastmod: 2024-02-19 22:04:58.470768
model: gpt-4-1106-preview
summary: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\u05DE\
  \u05EA\u05D0\u05D9\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA \u05D4\
  \u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05E1\u05D9\u05E0\
  \u05D5\u05E0\u05D9\u05DD \u05DE\u05D5\u05D7\u05DC\u05D9\u05DD \u05E2\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E1\u05D9\
  \u05E8 \u05EA\u05D5\u05D5\u05D9\u05DD \u05DC\u05D0 \u05E8\u05E6\u05D5\u05D9\u05D9\
  \u05DD. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E7\u05D5\u05EA \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD, \u05DC\u05D4\u05E4\u05D5\u05DA \u05E4\u05D5\u05E8\u05DE\u05D8\
  \ \u05DC\u05E7\u05D5\u05D4\u05E8\u05E0\u05D8\u05D9 \u05D0\u05D5\u2026"
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים שמתאימים לתבנית היא פעולה שבה סינונים מוחלים על מחרוזות כדי להסיר תווים לא רצויים. תכנתים עושים זאת כדי לנקות נתונים, להפוך פורמט לקוהרנטי או לאמת קלט.

## איך לעשות:
קוטלין מציעה את הפונקציה `replace` עם ביטויים רגולריים כדי להסיר תווים מתאימים. הנה דוגמא:

```Kotlin
fun main() {
    val pattern = Regex("[aeiou]")
    val result = "Kotlin Programming".replace(pattern, "")
    println(result) // Ktln Prgrmmng
}
```

הדוגמא מראה איך להסיר כל התווים המתאימים לתבנית - במקרה הזה, כל התווים שהם תנועות.

## צלילה עמוקה
ביטויים רגולריים (RegEx) היו בשימוש כבר בשנות ה-60 ככלי לביצוע פעולות על טקסט במחשבים. באמצעות ביטויים רגולריים, ניתן לבצע חיפוש והחלפה סופר-חזקה וגמישה של תווים במחרוזות.

בקוטלין, `replace` מופעלת על מחרוזות עם שני פרמטרים: הביטוי הרגולרי והמחרוזת שמחליפה את ההתאמות. אם רק רוצים לאתר את ההתאמות, אפשר להשתמש ב-`find` או ב-`findAll`.

גישה אלטרנטיבית היא להשתמש בפונקציות כמו `filter` שמחזירה רק תווים שעונים על תנאי מסוים, אבל זה נתפס כפחות יעיל למטרות של מחיקה.

## ראה גם
- [Regex in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Mastering Regular Expressions book](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
