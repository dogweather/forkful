---
title:                "Kotlin: מחיקת תווים התואמים דפוס"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

נמצאות רבות של פעמים שאנו צריכים למחוק תווים המתאימים לתבנית מסוימת מתוך מחרוזות שיש לנו. למשל, אולי יש לנו רשימת אימיילים ואנחנו רוצים להסיר את כל הישותות שאינן "gmail.com". במקרים כאלו, סילוק תווים המתאימים לתבנית הנתונה יכול לחסוך לנו מהרבה זמן וטרחה. בכתבה זו נכתוב על כיצד ניתן לכתוב פונקציה קצרה ויעילה ב-Kotlin שמוחקת תווים מתאימים לתבנית נתונה.

## איך לעשות זאת

הצעד הראשון הוא ליצור משתנה מסוג טקסט שיכיל את המבנה שבו אנחנו רוצים למחוק את התווים המתאימים. לדוגמה, במקרה שבו אנחנו רוצים למחוק את כל האימיילים שאינם "gmail.com", המשתנה יכול להיות "gmail.com". לאחר מכן, נשתמש בפונקציית replaceAll בכדי למחוק את התווים המתאימים לתבנית הנתונה. לעיל תוכלו למצוא דוגמא פשוטה של פונקציה המבצעת את הפעולה הנדרשת.

```Kotlin
fun deletePattern(string: String, pattern: String): String {
    return string.replaceAll(pattern, "")
}

println(deletePattern("test@gmail.com", "gmail.com")) // תמצא גם בפלט: test@
```

בדוגמא הבאה נציג פונקציה דומה, אך הפעם נעשה שימוש בפונקציית filter בכדי למחוק את התווים המתאימים לתבנית. פונקציית filter מחזירה רשימה של כל התווים שעונים על תנאי מסוים, ולאחר מכן משתמשים בפונקציית joinToString כדי ליצור מחרוזת חדשה בלי התווים המתאימים.

```Kotlin
fun deletePattern(string: String, pattern: String): String {
    return string.filter { it == pattern[0] }
        .joinToString("")
}

println(deletePattern("test@gmail.com", "gmail.com")) // תמצא גם בפלט: tes@.
```

כפי שאת