---
title:                "שרבוב מחרוזת"
date:                  2024-01-20T17:51:35.544874-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/interpolating-a-string.md"
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
