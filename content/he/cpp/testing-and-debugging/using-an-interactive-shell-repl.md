---
date: 2024-01-26 04:12:43.177950-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : C++ \u05DC\u05D0 \u05DE\u05D2\u05D9\u05E2\u05D4 \u05E2\u05DD REPL \u05DE\u05D5\
  \u05D1\u05E0\u05D9\u05EA, \u05D0\u05D1\u05DC \u05DB\u05DC\u05D9\u05DD \u05DB\u05DE\
  \u05D5 Cling \u05DE\u05E6\u05D9\u05E2\u05D9\u05DD \u05D9\u05DB\u05D5\u05DC\u05EA\
  \ \u05DB\u05D6\u05D0\u05EA. \u05D4\u05E0\u05D4 \u05D0\u05D9\u05DA \u05DC\u05D4\u05E9\
  \u05EA\u05DE\u05E9 \u05D1-Cling \u05DC\u05D7\u05D9\u05E9\u05D5\u05D1 \u05E1\u05DB\
  \u05D5\u05DD \u05E9\u05DC \u05E9\u05E0\u05D9 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  ."
lastmod: '2024-03-13T22:44:39.838808-06:00'
model: gpt-4-0125-preview
summary: "C++ \u05DC\u05D0 \u05DE\u05D2\u05D9\u05E2\u05D4 \u05E2\u05DD REPL \u05DE\
  \u05D5\u05D1\u05E0\u05D9\u05EA, \u05D0\u05D1\u05DC \u05DB\u05DC\u05D9\u05DD \u05DB\
  \u05DE\u05D5 Cling \u05DE\u05E6\u05D9\u05E2\u05D9\u05DD \u05D9\u05DB\u05D5\u05DC\
  \u05EA \u05DB\u05D6\u05D0\u05EA."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
weight: 34
---

## איך לעשות זאת:
C++ לא מגיעה עם REPL מובנית, אבל כלים כמו Cling מציעים יכולת כזאת. הנה איך להשתמש ב-Cling לחישוב סכום של שני מספרים:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "הסכום הוא: " << a + b << std::endl;
    return 0;
}

// פלט:
// הסכום הוא: 12
```

התחל את Cling והזן את הקוד שורה אחר שורה, תוך צפייה בפלט לאחר כל פקודה. זהו משוב מיידי, ללא צורך בקומפילציה.

## צלילה עמוקה
REPLs נפוצות עבור שפות כמו Python או Lisp, והן קיימות מאז שנות ה-60. עבור C++, שפה מקומפלת, המושג לא מתאים באופן טבעי, זו הסיבה שכלים כמו Cling קיימים - הם מפרשים את C++ בזמן אמת. חלופות כוללות קומפיילרים מקוונים או תכניות בדיקה קטנות שמקומפלות באופן מסורתי. Cling בנוי על גבי LLVM ו-Clang, מהווה גשר עבור C++ לשימוש באופן מפורש.

## ראה גם
- [Cling](https://root.cern/cling/): מפרש C++ אינטראקטיבי, בנוי על הספריות LLVM ו-Clang.
- [יפיתר נוטבוקס](https://jupyter.org/): מציעה קונסולה אינטראקטיבית בתוך סביבת נוטבוק, תומכת ב-C++ דרך הגרעין xeus-cling.
- [LLVM](https://llvm.org/): אוסף של טכנולוגיות קומפיילר וכלי תוכנה מודולריות וניתנות לשימוש חוזר, שעליהן Cling נבנה.
