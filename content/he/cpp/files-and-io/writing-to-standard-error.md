---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:26.310414-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-C++, \u05E0\
  \u05D9\u05EA\u05DF \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC\u05D6\u05E8\u05DD \u05D4\
  \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\
  \ \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05D6\u05E8\u05DD `cerr`, \u05D4\
  \u05D7\u05DC\u05E7 \u05DE\u05D4\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05D4\u05E1\
  \u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\
  \u05DE\u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA."
lastmod: '2024-03-13T22:44:39.863422-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-C++, \u05E0\u05D9\u05EA\u05DF \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC\
  \u05D6\u05E8\u05DD \u05D4\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\u05E1\u05D8\u05E0\
  \u05D3\u05E8\u05D8\u05D9 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05D6\
  \u05E8\u05DD `cerr`, \u05D4\u05D7\u05DC\u05E7 \u05DE\u05D4\u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05D4 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
weight: 25
---

## איך לעשות:
ב-C++, ניתן לכתוב לזרם השגיאה הסטנדרטי באמצעות הזרם `cerr`, החלק מהספרייה הסטנדרטית. הנה דוגמה בסיסית:

```cpp
#include <iostream>

int main() {
    // כתיבה לזרם הפלט הסטנדרטי
    std::cout << "This is a normal message." << std::endl;
    
    // כתיבה לזרם השגיאה הסטנדרטי
    std::cerr << "This is an error message." << std::endl;
    
    return 0;
}
```

פלט לדוגמה:
```
This is a normal message.
This is an error message.
```

במקרה זה, שתי ההודעות בדרך כלל יופיעו במסוף שלך, אך ניתן להכווין אותן בנפרד בשורת הפקודה. למשל, אתה יכול לשלוח פלט רגיל לקובץ בעוד ששגיאות יוצגו על המסך.

לניהול רישום ועיבוד שגיאות מתקדם יותר, ניתן להשתמש בספריות צד שלישי כמו `spdlog` או `boost.log`. ספריות אלו מציעות תכונות רבות לרישום, כולל עיצוב, רמות תיעוד, ופלט לקובץ.

הנה איך עשויים להשתמש ב-`spdlog` לכתוב הודעת שגיאה:

```cpp
#include "spdlog/spdlog.h"

int main() {
    // הפעלת spdlog
    spdlog::info("This is a normal message.");
    spdlog::error("This is an error message.");
    
    return 0;
}
```

שימו לב: לשימוש ב-`spdlog` יש להוסיף אותו לפרויקט שלכם. ניתן לעשות זאת על ידי שיכפול מאגר ה-GitHub או שימוש במנהל החבילות כמו `vcpkg` או `conan`.

זכרו, הבחירה בין השימוש בזרמים סטנדרטיים ישירות או בספרייה כמו `spdlog` תלויה במורכבות האפליקציה שלכם ובצרכים הספציפיים שלכם לגבי עיבוד שגיאות ורישום.
