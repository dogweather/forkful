---
title:                "כתיבה לשגיאה התקנית"
aliases:
- /he/arduino/writing-to-standard-error/
date:                  2024-02-03T19:33:44.344477-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

כתיבה לפלט השגיאה הסטנדרטי (stderr) בתכנות Arduino מערבת הפניית הודעות שגיאה ואבחונים לערוץ נפרד, בכדי להבטיח שהן לא יערבבו עם הפלט הסטנדרטי (stdout). מתכנתים עושים זאת כדי להבדיל בין פלטי תוכנית רגילים לבין הודעות שגיאה, מה שמקל על דיבאגינג וניתוח יומנים.

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
