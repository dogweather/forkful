---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:10.378167-07:00
description: "\u05D1\u05D4\u05E7\u05E9\u05E8 \u05E9\u05DC \u05EA\u05DB\u05E0\u05D5\
  \u05EA \u05DC-Arduino, \u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05E2\u05DC \u05DB\u05E8\u05D8\
  \u05D9\u05E1 SD \u05D0\u05D5 \u05DE\u05D5\u05D3\u05D5\u05DC \u05D0\u05D7\u05E1\u05D5\
  \u05DF \u05D3\u05D5\u05DE\u05D4 \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DA\
  \ \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D0\u05D5 \u05DC\u05DB\u05EA\u05D5\u05D1 \u05E7\
  \u05D1\u05E6\u05D9\u05DD \u05DC\u05DC\u05D0 \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  . \u05E4\u05E2\u05D5\u05DC\u05D4 \u05D6\u05D5 \u05D4\u05D9\u05D0 \u05D7\u05D9\u05D5\
  \u05E0\u05D9\u05EA \u05DC\u05EA\u05D9\u05E2\u05D5\u05D3\u2026"
lastmod: '2024-03-11T00:14:13.280147-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D4\u05E7\u05E9\u05E8 \u05E9\u05DC \u05EA\u05DB\u05E0\u05D5\u05EA\
  \ \u05DC-Arduino, \u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05E2\u05DC \u05DB\u05E8\u05D8\
  \u05D9\u05E1 SD \u05D0\u05D5 \u05DE\u05D5\u05D3\u05D5\u05DC \u05D0\u05D7\u05E1\u05D5\
  \u05DF \u05D3\u05D5\u05DE\u05D4 \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DA\
  \ \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D0\u05D5 \u05DC\u05DB\u05EA\u05D5\u05D1 \u05E7\
  \u05D1\u05E6\u05D9\u05DD \u05DC\u05DC\u05D0 \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  . \u05E4\u05E2\u05D5\u05DC\u05D4 \u05D6\u05D5 \u05D4\u05D9\u05D0 \u05D7\u05D9\u05D5\
  \u05E0\u05D9\u05EA \u05DC\u05EA\u05D9\u05E2\u05D5\u05D3\u2026"
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
בהקשר של תכנות ל-Arduino, בדיקה אם ספרייה קיימת על כרטיס SD או מודול אחסון דומה מאפשרת לך לקרוא או לכתוב קבצים ללא שגיאות. פעולה זו היא חיונית לתיעוד נתונים, ניהול הגדרות, או כל משימה שדורשת אחסון קבצים מובנה, ומבטיחה עמידות וביצועים זורמים ביישומים שלך.

## איך ל:
Arduino אינו תומך באופן טבעי בפעולות מערכת קבצים מורכבות מיד לאחר החלטתו. עם זאת, בעזרת שימוש בספריית SD, שהיא חלק מ-IDE הסטנדרטי של Arduino, תוכל לעבוד בקלות עם קבצים וספריות. כדי לבדוק אם ספרייה קיימת, אתה צריך להתחיל את הכרטיס SD ואז להשתמש בשיטת `exists()` מספריית SD.

ראשית, כלול את ספריית SD והגדר את חוט הבחירה של השבב:

```cpp
#include <SPI.h>
#include <SD.h>

const int chipSelect = 4; // חוט בחירת השבב עבור מודול כרטיס ה-SD
```

בפונקציית ה-`setup()` שלך, התחל את כרטיס ה-SD ובדוק אם הספרייה קיימת:

```cpp
void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(chipSelect)) {
    Serial.println("Initialization failed!");
    return;
  }

  // בדוק אם הספרייה קיימת
  if (SD.exists("/myDir")) {
    Serial.println("Directory exists.");
  } else {
    Serial.println("Directory doesn't exist.");
  }
}
```
בפונקציית `loop()`, אתה יכול להשאיר אותה ריקה או להוסיף קוד פעולה כפי שנדרש:

```cpp
void loop() {
  // קוד פעולה או להשאיר ריק
}
```

הפלט לדוגמה לאחר ריצת הקוד יהיה אחד מהשניים:

```
Directory exists.
```
או

```
Directory doesn't exist.
```

חשוב לוודא שכרטיס ה-SD מעוצב כראוי ושנתיב הספרייה `/myDir` מתאים לצרכים הספציפיים שלך. בדיקה זו היא אבן פינה לביצוע פעולות מורכבות יותר עם קבצים וספריות על כרטיסי SD עם Arduino.
