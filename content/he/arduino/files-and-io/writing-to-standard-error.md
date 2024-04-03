---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:44.344477-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Arduino \u05D0\u05D9\
  \u05E0\u05D4 \u05DE\u05D1\u05D7\u05D9\u05E0\u05D4 \u05DE\u05D4\u05D5\u05EA\u05D9\
  \u05EA \u05D1\u05D9\u05DF \u05E4\u05DC\u05D8 \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\
  \u05D9 \u05DC\u05E4\u05DC\u05D8 \u05E9\u05D2\u05D9\u05D0\u05D4 \u05DB\u05DE\u05D5\
  \ \u05DE\u05E2\u05E8\u05DB\u05D5\u05EA \u05D7\u05D9\u05E9\u05D5\u05D1 \u05E7\u05D5\
  \u05E0\u05D1\u05E0\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA. \u05E9\u05E0\
  \u05D9\u05D4\u05DD `Serial.print()` \u05D5-`Serial.println()` \u05DB\u05D5\u05EA\
  \u05D1\u05D9\u05DD \u05DC\u05D0\u05D5\u05EA\u05D5\u2026"
lastmod: '2024-03-13T22:44:39.794316-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u05D0\u05D9\u05E0\u05D4 \u05DE\u05D1\u05D7\u05D9\u05E0\u05D4 \u05DE\
  \u05D4\u05D5\u05EA\u05D9\u05EA \u05D1\u05D9\u05DF \u05E4\u05DC\u05D8 \u05E1\u05D8\
  \u05E0\u05D3\u05E8\u05D8\u05D9 \u05DC\u05E4\u05DC\u05D8 \u05E9\u05D2\u05D9\u05D0\
  \u05D4 \u05DB\u05DE\u05D5 \u05DE\u05E2\u05E8\u05DB\u05D5\u05EA \u05D7\u05D9\u05E9\
  \u05D5\u05D1 \u05E7\u05D5\u05E0\u05D1\u05E0\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\
  \u05EA."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
weight: 25
---

## איך לעשות:
Arduino אינה מבחינה מהותית בין פלט סטנדרטי לפלט שגיאה כמו מערכות חישוב קונבנציונליות. שניהם `Serial.print()` ו-`Serial.println()` כותבים לאותו פלט סריאלי, שבדרך כלל נצפה במוניטור הסריאלי של סביבת פיתוח Arduino. עם זאת, ניתן לחקות כתיבה ל-stderr על ידי עיצוב ספציפי של הודעות השגיאה או הפנייתן לפלט חלופי, כמו קובץ על כרטיס SD או דרך חיבור רשת.

כדי לחקות stderr, תוכלו להקדים תגיות להודעות שגיאה כמו "ERROR:" כדי להבדיל זאת במוניטור הסריאלי:

```cpp
void setup() {
  Serial.begin(9600); // אתחול תקשורת סריאלית בשיעור בוד 9600
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // חיקוי stderr על ידי הקדמת הודעת שגיאה
    Serial.println("ERROR: הפונקציה נכשלה בביצוע.");
  } else {
    Serial.println("הפונקציה בוצעה בהצלחה.");
  }
  delay(1000); // המתנה שנייה לפני התחלה מחדש של הלולאה
}

int someFunction() {
  // פונקציה חסרת משמעות שמחזירה -1 בשגיאה
  return -1;
}
```

פלט לדוגמה במוניטור הסריאלי של סביבת פיתוח Arduino עשוי להיראות כך:

```
ERROR: הפונקציה נכשלה בביצוע.
```

לפרויקטים הדורשים גישה מתוחכמת יותר, כולל כתיבה לפלטים פיזיים שונים, יהיה נחוץ השימוש בספריות צד שלישי או בחומרה נוספת. לדוגמה, כתיבת הודעות שגיאה לכרטיס SD דורשת את ספריית ה-`SD`:

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("ERROR: אתחול כרטיס ה-SD נכשל!");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("ERROR: הפונקציה נכשלה בביצוע.");
    myFile.close(); // חשוב לסגור את הקובץ כדי לשמור את התוכן
  } else {
    Serial.println("ERROR: פתיחת error.log נכשלה!");
  }
}

void loop() {
  // הקוד העיקרי שלך ייכתב כאן
}
```

עם גישה זו, אתם מפרידים פיזית בין פלט התוכנית הרגיל להודעות שגיאה על ידי הפניית האחרונות לקובץ `error.log` על כרטיס SD, מה שמאפשר אנליזות פוסט מורטם מבלי לבלבל את ערוץ הפלט הראשי.
