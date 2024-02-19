---
aliases:
- /he/kotlin/interpolating-a-string/
date: 2024-01-20 17:51:35.544874-07:00
description: "\u05DE\u05D9\u05DC\u05D5\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D4\u05D5\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05EA\
  \u05D4 \u05DE\u05E9\u05DC\u05D1 \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD \u05D5\u05D1\
  \u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D9\u05D9\u05E6\u05E8\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D3\u05D9\u05E0\u05DE\u05D9 \u05D1\u05E7\u05DC\u05D5\
  \u05EA, \u05DC\u05DC\u05D0 \u05E6\u05D5\u05E8\u05DA \u05D1\u05D7\u05D9\u05D1\u05D5\
  \u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1\u05E6\u05D5\u05E8\u05D4\
  \ \u05DE\u05E1\u05D5\u05E8\u05D1\u05DC\u05EA."
lastmod: 2024-02-18 23:08:52.782316
model: gpt-4-1106-preview
summary: "\u05DE\u05D9\u05DC\u05D5\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D4\u05D5\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05EA\
  \u05D4 \u05DE\u05E9\u05DC\u05D1 \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD \u05D5\u05D1\
  \u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D9\u05D9\u05E6\u05E8\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D3\u05D9\u05E0\u05DE\u05D9 \u05D1\u05E7\u05DC\u05D5\
  \u05EA, \u05DC\u05DC\u05D0 \u05E6\u05D5\u05E8\u05DA \u05D1\u05D7\u05D9\u05D1\u05D5\
  \u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1\u05E6\u05D5\u05E8\u05D4\
  \ \u05DE\u05E1\u05D5\u05E8\u05D1\u05DC\u05EA."
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
מילוי מחרוזות הוא תהליך שבו אתה משלב משתנים וביטויים בתוך מחרוזת. תכנתים עושים את זה כדי לייצר טקסט דינמי בקלות, ללא צורך בחיבור מחרוזות בצורה מסורבלת.

## איך לעשות:
קודטלין נותן לך דרך נקייה ויעילה לעבוד עם מילוי מחרוזות באמצעות תבניות מחרוזת. הנה דוגמא:

```kotlin
fun main() {
    val name = "ישראל"
    val age = 30
    println("שלום, שמי $name ואני בן $age שנים.")
    // או עם ביטויים
    println("בעוד חמש שנים אהיה בן ${age + 5}.")
}
```

תוצאת ההדפסה:

```
שלום, שמי ישראל ואני בן 30 שנים.
בעוד חמש שנים אהיה בן 35.
```

## עיון נוסף:
היסטורית, תכניתנים היו צריכים להשתמש באופרטורים ליצירת מחרוזת חדשה ממספר מחרוזות קיימות, שזה יכול להיות מסורבל. עם הזמן, מילוי מחרוזות (String Interpolation) הפך לתכונה נפוצה בשפות תכנות. בקוטלין, מילוי מחרוזות מכונה "String Templates" ומאפשר שילוב ביטויים בתוך מחרוזות באופן אינטואיטיבי. אלטרנטיבות כוללות שימוש ב-`String.format()` או בנייה של `StringBuilder`, אבל לרוב אין צורך בגישות מסורבלות אלה.

## ראה גם:
- מדריך התחלה מהיר ל- Kotlin: https://kotlinlang.org/docs/reference/basic-syntax.html#using-string-templates
- תיעוד רשמי על תבניות מחרוזת: https://kotlinlang.org/docs/reference/basic-types.html#string-templates
