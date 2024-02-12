---
title:                "בדיקה אם ספרייה קיימת"
aliases:
- /he/arduino/checking-if-a-directory-exists/
date:                  2024-02-03T19:07:10.378167-07:00
model:                 gpt-4-0125-preview
simple_title:         "בדיקה אם ספרייה קיימת"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
