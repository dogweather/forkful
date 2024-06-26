---
date: 2024-01-26 01:18:43.099766-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D3\u05DE\u05D9\
  \u05D9\u05E0\u05D5 \u05E9\u05D9\u05E9 \u05DC\u05DB\u05DD \u05E4\u05D5\u05E0\u05E7\
  \u05E6\u05D9\u05D4 \u05E9\u05E2\u05D5\u05E9\u05D4 \u05E7\u05E6\u05EA \u05D9\u05D5\
  \u05EA\u05E8 \u05DE\u05D3\u05D9, \u05DB\u05DE\u05D5 \u05D4\u05DE\u05EA\u05D5\u05D3\
  \u05D4 \u05D4\u05DE\u05D5\u05D2\u05D1\u05DC\u05EA \u05D4\u05D6\u05D5 \u05E9\u05DE\
  \u05D0\u05EA\u05D7\u05DC\u05EA \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 \u05D5\
  \u05D2\u05DD \u05DE\u05D1\u05E6\u05E2\u05EA \u05EA\u05D9\u05E2\u05D5\u05D3."
lastmod: '2024-03-13T22:44:39.849971-06:00'
model: gpt-4-0125-preview
summary: "\u05D3\u05DE\u05D9\u05D9\u05E0\u05D5 \u05E9\u05D9\u05E9 \u05DC\u05DB\u05DD\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05E9\u05E2\u05D5\u05E9\u05D4 \u05E7\
  \u05E6\u05EA \u05D9\u05D5\u05EA\u05E8 \u05DE\u05D3\u05D9, \u05DB\u05DE\u05D5 \u05D4\
  \u05DE\u05EA\u05D5\u05D3\u05D4 \u05D4\u05DE\u05D5\u05D2\u05D1\u05DC\u05EA \u05D4\
  \u05D6\u05D5 \u05E9\u05DE\u05D0\u05EA\u05D7\u05DC\u05EA \u05D0\u05D5\u05D1\u05D9\
  \u05D9\u05E7\u05D8 \u05D5\u05D2\u05DD \u05DE\u05D1\u05E6\u05E2\u05EA \u05EA\u05D9\
  \u05E2\u05D5\u05D3."
title: "\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2"
weight: 19
---

## איך לעשות:
דמיינו שיש לכם פונקציה שעושה קצת יותר מדי, כמו המתודה המוגבלת הזו שמאתחלת אובייקט וגם מבצעת תיעוד:

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // לוגיקת אתחול
        // ...

        // תיעוד מפורט
        if (verbose) {
            std::cout << "Widget initialized!" << std::endl;
        }
    }
};

// שימוש:
Widget w;
w.init(true);
```

פלט:
```
Widget initialized!
```

רפקטורינג של זה למתודות נקיות וממוקדות יותר עשוי להיראות כך:

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // רק לוגיקת אתחול
        // ...
    }

    void logInitialization() const {
        std::cout << "Widget initialized!" << std::endl;
    }
};

// שימוש:
Widget w;
w.init();
w.logInitialization();
```

שינוי זה לא שינה את מה שהתוכנית עושה אך הופך את הכיתה `Widget` למודולרית יותר ואת שימושה לברור יותר.

## צלילה לעומק
המושג של רפקטורינג כפי שאנחנו מכירים אותו היום נולד בקהילות התכנות של Smalltalk בשנות ה-80 והופך לפופולרי ביותר על ידי ספרו של מרטין פאולר "Refactoring: Improving the Design of Existing Code" מ-1999. היום, רפקטורינג הוא חלק בלתי נפרד מפיתוח תוכנה מודרני, משולב בשיטות פיתוח שונות כמו Agile ו-TDD (Test-Driven Development).

כשאנחנו מדברים על אלטרנטיבות לרפקטורינג, אנחנו נכנסים לתחום של כתיבה מחדש או עיצוב מחדש. רפקטורינג הוא אסטרטגי והדרגתי, בעוד שכתיבה מחדש עשויה להיפטר מהקוד הקיים לטובת פתרון חדש. עיצוב מחדש, מאידך, עשוי לכלול שינויים משמעותיים יותר כולל שינויי פונקציונליות, שאינם מטרה לצורך רפקטורינג טהור.

פרטי היישום של רפקטורינג יכולים להיות די מפורטים. ישנם רבים 'ריחות קוד' שעשויים להצביע על צורך ברפקטורינג, כמו מתודות ארוכות, כיתות גדולות, או קוד משוכפל. קיימים כלים אוטומטיים שיכולים לסייע ברפקטורינג, כמו "קלנג-טיידי" עבור C++, שיכול לזהות בעיות ואף להחיל תיקונים.

בנוסף, רפקטורינג דורש מערכת מבדקים מוצקה כדי להבטיח שהפונקציונליות נשארת ללא שינוי. בלי מבדקים, אתם בעצם טסים בעיוורון ומסכנים את עצמכם לרגרסיות.

## ראו גם
להבנה עמוקה יותר של רפקטורינג ולראות דוגמאות נוספות, תרצו אולי לבדוק:

- הטקסט הקלאסי של מרטין פאולר "Refactoring: Improving the Design of Existing Code" לרעיונות ואסטרטגיות בסיסיים.
- את התיעוד של `Clang-Tidy` בכתובת https://clang.llvm.org/extra/clang-tidy/ לתמיכה אוטומטית ברפקטורינג ב-C++.
- "Working Effectively with Legacy Code" מאת מייקל פתרס, שמספק טכניקות לרפקטורינג בטוח בהקשר של בסיסי קוד פחות מושלמים.
