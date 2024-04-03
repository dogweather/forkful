---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:32.396230-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D3\u05D9\
  \ \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05E2\u05DC \u05DB\u05E8\u05D8\u05D9\u05E1 SD \u05D1\u05D0\u05DE\u05E6\u05E2\
  \u05D5\u05EA \u05D0\u05E8\u05D3\u05D5\u05D0\u05D9\u05E0\u05D5, \u05E8\u05D0\u05E9\
  \u05D9\u05EA \u05E2\u05DC\u05D9\u05DA \u05DC\u05DB\u05DC\u05D5\u05DC \u05D0\u05EA\
  \ \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4-`SD.h`, \u05D4\u05DE\u05E1\u05E4\u05E7\
  \u05EA \u05D0\u05EA \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05D4\
  \u05D3\u05E8\u05D5\u05E9\u05D5\u05EA \u05DC\u05D0\u05D9\u05E0\u05D8\u05E8\u05E7\u05E6\
  \u05D9\u05D4 \u05E2\u05DD \u05DB\u05E8\u05D8\u05D9\u05E1\u05D9 SD.\u2026"
lastmod: '2024-03-13T22:44:39.797632-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC\u05E7\u05D5\u05D1\
  \u05E5 \u05D8\u05E7\u05E1\u05D8 \u05E2\u05DC \u05DB\u05E8\u05D8\u05D9\u05E1 SD \u05D1\
  \u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D0\u05E8\u05D3\u05D5\u05D0\u05D9\u05E0\u05D5\
  , \u05E8\u05D0\u05E9\u05D9\u05EA \u05E2\u05DC\u05D9\u05DA \u05DC\u05DB\u05DC\u05D5\
  \u05DC \u05D0\u05EA \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4-`SD.h`, \u05D4\u05DE\
  \u05E1\u05E4\u05E7\u05EA \u05D0\u05EA \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\
  \u05D5\u05EA \u05D4\u05D3\u05E8\u05D5\u05E9\u05D5\u05EA \u05DC\u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E7\u05E6\u05D9\u05D4 \u05E2\u05DD \u05DB\u05E8\u05D8\u05D9\u05E1\
  \u05D9 SD."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 24
---

## איך לעשות:
כדי לכתוב לקובץ טקסט על כרטיס SD באמצעות ארדואינו, ראשית עליך לכלול את ספריית ה-`SD.h`, המספקת את הפונקציות הדרושות לאינטרקציה עם כרטיסי SD. ודא שלוח הארדואינו שלך מחובר למודול כרטיס SD.

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // אתחול תקשורת סריאלית ב-9600 סיביות לשנייה:
  Serial.begin(9600);
  
  // בדוק את אתחול כרטיס ה-SD
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  Serial.println("Initialization done.");
  
  // פתח את הקובץ. שים לב שניתן לפתוח קובץ אחד בכל פעם,
  // לכן עליך לסגור אותו לפני שתפתח אחר.
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // אם הקובץ נפתח בהצלחה, כתוב אליו:
  if (myFile) {
    Serial.print("Writing to test.txt...");
    myFile.println("Testing text file write.");
    // סגור את הקובץ:
    myFile.close();
    Serial.println("done.");
  } else {
    // אם הקובץ לא נפתח, הדפס שגיאה:
    Serial.println("Error opening test.txt");
  }
}

void loop() {
  // לאחר הכנת המערכת לא קורה דבר
}
```

### פלט לדוגמה:
כאשר אתה מריץ את הקוד הזה, ה Serial Monitor של סביבת הפיתוח של ארדואינו יציג:
```
Initialization done.
Writing to test.txt...done.
```
כדי לבדוק אם הנתונים נכתבו באופן תקין, תוכל להוציא את כרטיס ה-SD מהארדואינו, להכניס אותו למחשב, ולפתוח את הקובץ `test.txt` כדי לראות את ההודעה "Testing text file write."

לפרויקטים הדורשים פעולות קובץ מתקדמות יותר או עיבוד, שקול לחקור ספריות נוספות או לכתוב פונקציות מותאמות אישית המתאימות לצרכים הספציפיים שלך.
