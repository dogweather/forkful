---
title:                "Kotlin: שימוש בביטויים רגולריים"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

# למה
כדי לפתור בעיות קשות יותר בקוד, כמו חיפוש והחלפה של מחרוזות ספציפיות.

## איך לעשות זאת
תחילה, צריך לייבא את הספרייה הנדרשת לביצוע פעולות עם Regular Expressions ב-Kotlin:
```
import kotlin.text.*
```
אחר כך, ניתן להשתמש בפקודת החיפוש למצוא מחרוזת ספציפית בתוך מחרוזת אחרת:
```
val text = "זהו טקסט לדוגמה"
val pattern = Regex("טקסט")
val matchResult = pattern.find(text)
```
הפריט `matchResult` יחזיר את המחרוזת המתאימה הראשונה שמצא בתוך הטקסט. ניתן להשתמש גם בפקודה `findAll()` כדי למצוא את כל המחרוזות התואמות.

עבור החלפת מחרוזות, ניתן לעשות כך:
```
val text = "מחרוזת להחלפה"
val newText = text.replace("מחרוזת", "מחרוזת חדשה")
```
הפריט `newText` יחזיר את המחרוזת המעודכנת עם המחרוזת החדשה שהוחלפה.

## דפדוף מעמיק
כדי להיות יעילים בשימוש ב-Regular Expressions, יש ללמוד כיצד ליצור תבניות מתאימות וכיצד להשתמש בתוויתי Quantifiers כדי למצוא את המחרוזות הרלוונטיות. ניתן למצוא מידע נוסף על זה בדף האינטרנט של Kotlin על Regular Expressions.

## ראה גם
- [מדריך רשמי על כיצד להשתמש ב-Regular Expressions ב-Kotlin](https://kotlinlang.org/docs/regex.html)
- [מהי ה-Regex של Kotlin?](https://www.baeldung.com/kotlin-regex)
- [Regular Expressions Cheat Sheet ל-Kotlin](https://www.programcreek.com/regex/kotlin-regex-cheat-sheet)