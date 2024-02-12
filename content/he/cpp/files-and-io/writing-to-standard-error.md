---
title:                "כתיבה לשגיאה התקנית"
aliases:
- /he/cpp/writing-to-standard-error/
date:                  2024-02-03T19:33:26.310414-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

כתיבה לזרם שגיאה הסטנדרטי (`stderr`) ב-C++ כוללת הפצת הודעות שגיאה או אבחונים הנפרדים מפלט התוכנית הראשי. מתכנתים עושים זאת כדי להכווין שגיאות לזרם שונה, מה שמאפשר לנפרד בין פלט רגיל לבין הודעות שגיאה, ובכך להקל על ניפוי באגים ועיבוד שגיאות.

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
