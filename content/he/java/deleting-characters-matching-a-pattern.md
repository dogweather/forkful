---
title:                "Java: מחיקת תווים התואמים תבנית"
simple_title:         "מחיקת תווים התואמים תבנית"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# למה
תכנות Java הינו לא רק יכולת טכנית, אלא גם כישור יוצא דופן לפתור בעיות באופן יצירתי. מחיקת תווים המתאימים לתבנית היא כלי עוצמתי שיכול לשפר את תהליך השיפור והתקלות בקוד.

## איך לעשות זאת
לפניכם ישנם כמה דרכים שונות למחוק תווים המתאימים לתבנית בתוך כתובת דואר או בטקסט מסוים. להלן דוגמאות של קוד Java המתאימות למצבים שונים:

```
// למחיקת תווים מתאימים לתבנית בתוך כתובת דואר יש להשתמש בפונקציית "replace" ולהגדיר בתוכה את התבנית עליה תתבסס המחיקה
String email = "example@domain.com";
String newEmail = email.replace("@", "");
// את התו המתאים ניתן להחליף גם בתו ריק כדי למחוק אותו לחלוטין
```

```
// בטקסט מסוים ניתן לעשות שימוש בלולאה כדי לסרבול את כל התווים המתאימים לתבניות מסוימות
String text = "Hello world!";
// במקרה הזה, נמחק את כל התווים המתאימים לסיווג ה-BD מתוך הטקסט
for (int i = 0; i < text.length(); i++) {
    char currentChar = text.charAt(i);
    if (currentChar == 'H' || currentChar == 'o' || currentChar == 'l') {
        text = text.replace(currentChar, "");
    }
}
// לאחריה המשתנה יכיל את הטקסט "e wrd!", ללא התווים המתאימים לתבנית.
```

## מכורך לעומק
בנוסף לשימוש בפונקציות המובנות ב-Java כדי למחוק תווים לפי תבנית, ניתן להשתמש גם בספריות חיצוניות. על פי התבנית שנבחרת, ייתכן שספריות כמו Regex או StringUtils יהיו יעילות יותר ותורמות לייצור קוד נקי ומנומס.

# ראו גם
- [פונקציית "replace" ב