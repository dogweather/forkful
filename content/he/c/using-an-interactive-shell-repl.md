---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
date:                  2024-01-26T04:12:10.386541-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## מה ולמה?
מעטפת אינטרקטיבית, או לולאת קריאה-הערכה-הדפסה (REPL), היא כלי המספק סביבת קידוד בזמן אמת כדי לבדוק קטעי קוד באופן מיידי. תכנתים משתמשים בה לקבלת משוב מהיר במהלך הפיתוח, הלמידה והדיבאג.

## איך לעשות:
C אינו מציע REPL מובנה, אך ניתן להשתמש בכלים של צד שלישי. הנה הצצה באמצעות Cling, מפרש C++ שיכול גם לטפל בקוד C:

```C
#include <stdio.h>

int main() {
    printf("Hello, REPL world!\n");
    return 0;
}
```

פלט ב-REPL של Cling:
```
[cling]$ .x yourscript.c
Hello, REPL world!
```

Cling מריץ את הסקריפט ומדפיס את הפלט באופן מיידי.

## עיון עמוק
REPLs הם סטנדרטיים בשפות דינמיות כמו Python או Ruby, אך לגבי שפות מקומפלות כמו C, הם פחות נפוצים. היסטורית, מחזור הקומפילציה-הרצה-דיבאג לא התאים לחקר אינטרקטיבי. כלים כמו Cling ומקומפלרי C מקוונים מציעים חוויות דמויות REPL על ידי הכללת הקוד שלך בסביבת C++.

חלופות ל-Cling כוללות מפרשי C כמו CINT ו-Ch. כלים אלו מאפשרים איטרציה מהירה אך עשויים לא להתאים לכל תרחישי הפיתוח בשל מגבלות ביצועים ותמיכה בתכונות מורכבות.

הטמעת REPL בשפה מקומפלת כוללת קומפילציה והרצה של קטעי קוד במרחב, הנחשבת ללא טריוויאלית ועשויה להיות מוגבלת ביחס ליכולות השפה המלאות.

## ראה גם
- Cling: https://github.com/root-project/cling
- מקומפלר ו-REPL מקוונים ל-C: https://repl.it/languages/c
- CINT: http://root.cern.ch/drupal/content/cint
- מפרש Ch: http://www.softintegration.com/products/chstandard/