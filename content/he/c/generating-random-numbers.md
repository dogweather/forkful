---
title:    "C: יצירת מספרים אקראיים"
keywords: ["C"]
---

{{< edit_this_page >}}

# למה

כל אחד מאיתנו כנראה נתקל במצבים שבהם הופכנו לחלוטין להסתמכות על מספרים אקראיים. יתכן שאתה צריך מספר אקראי לבדוק היעילות של אלגוריתם, אולי אתה צריך ליצור קובייה ורוצה שהתוצאות שלך יוצאות נראות כ-נדיב ככל האפשר, או אולי אתה רוצה להשתמש במספרים אקראיים כקלט לצורך יצירת משחקים. בקיצור, ישנם מגוון רב של מצבים בהם ייתכן שתצטרך ליצור מספרים אקראיים. מחשבון תזמונים עד משחקי מחשב מוכרים, לא מצונזר ורנדומליים. ובאמצעות קוד בשפת C ניתן ליצור מספרים אקראיים בקלות. בואו נראה איך ניתן לעשות זאת!

# כיצד לעשות

השתמשו בקוד הבא כדי ליצור מספרים אקראיים בקוד C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    /* התחלת גרירה עבור מחשבונים טבעיים */
    srand(time(0));

    /* הדפסת 5 מספרים אקראיים בין 0 ל-100 */
    for(int i = 0; i < 5; i++)
        printf("%d ", rand()%101);

    return 0;
}
```

תוצאות הרצה:

```
13 85 41 24 98
```

בשורות הבאות, נכיר את כל הפקודות שנמצאות בקוד הזה:

- הפקודה `#include <stdio.h>` מאפשרת לנו להשתמש בפונקציות קלט ופלט מהספרייה הסטנדרטית של C.
- הפקודה `#include <stdlib.h>` מאפשרת לנו להשתמש בפונקציות כמו `rand()` ו- `srand()` שמסייעות לנו ליצור מספרים אקראיים.
- הפקודה `#include <time.h>` מאפשרת לנו להשתמש בפונקציה `time()` שמספקת לנו את הזמן הנוכ