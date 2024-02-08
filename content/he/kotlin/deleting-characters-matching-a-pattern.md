---
title:                "מחיקת תווים התואמים לתבנית"
aliases:
- he/kotlin/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:04.612503-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/deleting-characters-matching-a-pattern.md"
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
