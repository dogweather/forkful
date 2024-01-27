---
title:                "כתיבה לקובץ טקסט"
date:                  2024-01-19
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט היא יצירת קבצים אשר מכילים טקסט נקרא. תכניתנים עושים זאת לשמירת נתונים, לוגים, והגדרות.

## איך לעשות:
```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);

  if (!SD.begin(4)) {
    Serial.println("איתחול כרטיס SD נכשל");
    return;
  }

  myFile = SD.open("test.txt", FILE_WRITE);

  if (myFile) {
    myFile.println("שלום, עולם!");
    myFile.close(); // אל תשכח לסגור את הקובץ
  } else {
    Serial.println("שגיאה בפתיחת הקובץ");
  }
}

void loop() {
  // לא נדרשת פונקציונליות בלולאה עבור דוגמת זו
}
```
**תוצאה:** יצירת קובץ `test.txt` על כרטיס SD עם המחרוזת "שלום, עולם!".

## עיון מעמיק
בהקשר ההיסטורי, כתיבת קובצים הייתה מורכבת יותר לפני חיבוריות SD ו-EEPROM נפוצות עבור ארדואינו. חלופות עומדות לרשותנו כוללות כתיבה ל-EEPROM או שימוש בשרתי ענן. מימוש כתיבת הקובץ דורש התמודדות עם גודל הקובץ ושימור מקום בזיכרון.

## ראו גם
- [מדריך למודולים של כרטיסי SD לארדואינו](https://www.arduino.cc/en/Reference/SD)
- [תיעוד פקודות FILE של Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [ארדואינו: כתיבה וקריאה מ-EEPROM](https://www.arduino.cc/en/Tutorial/LibraryExamples/EEPROMWrite)
