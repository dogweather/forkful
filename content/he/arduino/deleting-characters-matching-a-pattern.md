---
title:    "Arduino: מחיקת תווים התואמים לתבנית"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## למה

בעולם התכנות, כיתוב תכניות נוטה להיות מורכב משפה שפתיתה ומסובכת. לפעמים זה יכול להיות מעט מבלבל למתחילים ואף למתכנתים מנוסים. אחת המשימות הנפוצות בתכנות היא למחוק תווים מסוימים התואמים לתבנית מסוימת. זה יכול להיות מועיל במצבים שבהם עלינו לטפל בנתונים לפני שאנו יכולים להשתמש בהם.

## איך לעשות זאת?

למרבה המזל, תכנתי ה-Arduino מספקת לנו כלים נהדרים כדי לעזור לנו לעשות זאת מהיר וקל. באמצעות פונקציית `replace()` ניתן למחוק תווים מתוך מחרוזת לפי תבנית נתונה. הנה כמה דוגמאות קצרות שיעזרו להבין את השימוש בפונקציה זו:

```arduino
// מחק את האות "א" מתוך המחרוזת
String myString = "שלום ארדואינו";
myString.replace("א", "");
// פלט: שלום רדואינו

// מחק את כל האותיות העבריות מתוך המחרוזת
String myString = "מבנה העמק";
myString.replace("א-ת", "");
// פלט: ה

// מחק את כל המספרים מתוך המחרוזת
String myString = "1234 שלום";
myString.replace("0-9", "");
// פלט:  שלום
```

כמו שאתם רואים, אפשר להשתמש בפונקציה כדי למחוק תווים מתוך מחרוזת במגוון של תבניות. ניתן לראות שימוש נוסף ל־`replace()` בדוגמה הבאה, כאשר אנו מוחקים את כל האותיות הקפיטליות מתוך מחרוזת מסוימת:

```arduino
// מחק את כל האותיות הקפיטליות מתוך המחרוזת
String myString = "Hello World";
myString.replace("A-Z", "");
// פלט: ello orld
```

כאן אתם יכולים לראות שימוש נוסף ל־`replace()` כאשר אנו משתמ