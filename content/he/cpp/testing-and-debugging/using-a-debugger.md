---
date: 2024-01-26 03:48:41.499504-07:00
description: "\u05D4\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4\
  \ \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05DE\u05D0\u05E4\u05E9\u05E8 \u05D4\u05E4\
  \u05E2\u05DC\u05D4 \u05E9\u05DC \u05DB\u05DC\u05D9 \u05E9\u05DE\u05D0\u05E4\u05E9\
  \u05E8 \u05DC\u05DA \u05DC\u05D4\u05E6\u05D9\u05E5 \u05DC\u05EA\u05D5\u05DA \u05D4\
  \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05D4\u05E8\u05E6\u05D4 \u05E9\u05DC\u05DA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D9\u05DF \u05DE\u05D4 \u05D1\u05D0\u05DE\
  \u05EA \u05E7\u05D5\u05E8\u05D4. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05DE\u05E6\u05D5\
  \u05D0 \u05D5\u05DC\u05DC\u05D7\u05D5\u05E5 \u05D7\u05E8\u05E7\u05D9\u05DD - \u05D1\
  \u05E2\u05D9\u05D5\u05EA\u2026"
lastmod: 2024-02-19 22:04:59.106806
model: gpt-4-0125-preview
summary: "\u05D4\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\
  \u05D2\u05D9\u05D0\u05D5\u05EA \u05DE\u05D0\u05E4\u05E9\u05E8 \u05D4\u05E4\u05E2\
  \u05DC\u05D4 \u05E9\u05DC \u05DB\u05DC\u05D9 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8\
  \ \u05DC\u05DA \u05DC\u05D4\u05E6\u05D9\u05E5 \u05DC\u05EA\u05D5\u05DA \u05D4\u05EA\
  \u05D5\u05DB\u05E0\u05D9\u05EA \u05D4\u05E8\u05E6\u05D4 \u05E9\u05DC\u05DA \u05DB\
  \u05D3\u05D9 \u05DC\u05D4\u05D1\u05D9\u05DF \u05DE\u05D4 \u05D1\u05D0\u05DE\u05EA\
  \ \u05E7\u05D5\u05E8\u05D4. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05DE\u05E6\u05D5\u05D0\
  \ \u05D5\u05DC\u05DC\u05D7\u05D5\u05E5 \u05D7\u05E8\u05E7\u05D9\u05DD - \u05D1\u05E2\
  \u05D9\u05D5\u05EA\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
השימוש במנפה שגיאות מאפשר הפעלה של כלי שמאפשר לך להציץ לתוך התוכנית הרצה שלך כדי להבין מה באמת קורה. תכנתים עושים זאת כדי למצוא וללחוץ חרקים - בעיות מציקות אשר גורמות לקוד שלך להתנהג בצורה בלתי צפויה או להתמוטט.

## איך לעשות:
C++ משתלב עם מנפי שגיאות כמו GDB או מנפה השגיאות של Visual Studio. הנה דוגמה קצרצרה בשימוש ב-GDB:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 0;
    int c = a / b; // אופס, חלוקה באפס!
    std::cout << c << std::endl;
    return 0;
}

// קמפול עם:
// g++ -g -o my_program my_program.cpp

// הרץ עם מנפה שגיאות:
// gdb ./my_program
```

לאחר שהתחלת את GDB, תוכל להגדיר נקודות עצירה, לעבור דרך הקוד שלך, לבדוק משתנים ועוד הרבה. אם תריץ את הדוגמה למעלה, תראה שהתוכנית שלך קורסת בשל החלוקה באפס.

## צלילה עמוקה
ניפוי שגיאות שורשיו בימים הראשונים של התכנות, שם היה צורך להסיר באופן ממשי חרקים (בעלי חיים!) מהחומרה. מאז, כלי ניפוי שגיאות התפתחו לתוכנה מורכבת וחזקה, קריטית לפיתוח.

חלופות ל-GDB עבור C++ כוללות את LLDB, כמו גם מנפי שגיאות משולבי סביבת פיתוח, כמו אלו ב-Visual Studio, CLion, או Eclipse. סביבות מודרניות אלו מספקות ממשקים גרפיים אשר הופכים את ניפוי השגיאות לפחות מפחיד.

פרטי היישום אודות שימוש במנפה שגיאות תלויים לעיתים בסביבת הפיתוח שלך:

- מנפי שגיאות מבוססי שורת פקודה (GDB, LLDB) דורשים היכרות עם פקודות טרמינל ולעיתים כוללים מעקב למידה תלול יותר.
- מנפי שגיאות גרפיים מפשטים את התהליך על ידי אפשרות להפעלה ולחיצה כדי להגדיר נקודות עצירה, לעבור דרך הקוד ולעקוב אחרי משתנים.

הבנת יכולותיו של המנפה שגיאות שלך, כמו נקודות עצירה מותנות, נקודות מעקב, או הערכת ביטויים, יכולה לשפר באופן משמעותי את יעילותך באבחנת בעיות.

## ראה גם
- [תיעוד GDB](https://www.gnu.org/software/gdb/documentation/)
- [תיעוד פקודות LLDB](https://lldb.llvm.org/use/map.html)
- [מדריך למנפה שגיאות של Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/debugger-feature-tour)
- [ניפוי שגיאות עם CLion](https://www.jetbrains.com/help/clion/debugging-code.html)
