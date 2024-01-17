---
title:                "יצירת קובץ זמני"
html_title:           "Arduino: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני הוא פעולה שתיוגרמה למחשב לאחסן מידע זמני בזמן ריצת התוכנית. פעולה זו חשובה לתכנותנים לצורך שמירת מידע זמני שיכול לשמש במהלך התכנית.

## איך לעשות זאת:
כדי ליצור קובץ זמני ב-Arduino, יש להשתמש בפונקציות `File tempFile = SPIFFS.open("/tempFile.txt", "w+");` ו-`tempFile.close();` כדי ליצור ולסגור את הקובץ הזמני בהתאמה. להלן דוגמא פשוטה של קוד ופלט תוצאה עבור קובץ זמני ב-Arduino: 
```
// יצירת קובץ זמני בספריית SPIFFS
#include <SPIFFS.h> 

void setup() {
  Serial.begin(9600); // הפעלת הכנסת נתונים למשתנה זמני
  File tempFile = SPIFFS.open("/tempFile.txt", "w+"); // יצירת קובץ זמני בשם "tempFile.txt"
  tempFile.print("מידע זמני של התכנית"); // הקלדת המידע הזמני בקובץ
  tempFile.close(); // סגירת הקובץ
  tempFile = SPIFFS.open("/tempFile.txt", "r"); // פתיחת הקובץ לצורך קריאת המידע הזמני
  if (tempFile) {
    while (tempFile.available()) { // כל עוד יש מידע זמין בקובץ
      Serial.write(tempFile.read()); // הצגת המידע הזמני בטיחות גלישה
    }
    tempFile.close(); // סגירת הקובץ
  } 
  else {
    Serial.println("לא מצליח לפתוח את הקובץ הזמני");
  }
}

void loop() {
  // פעולות נוספות לאחר הצגת המידע הזמני
}
```

פלט תוצאה: `מידע זמני של התכנית`

## מעמד עמוק:
פעולת היצירה של קובץ זמני נמצאת בשימוש מזה עשורים רבים והיא נחשבת לפעולה יסודית בתכנות. פעולה זו ניתנת לשימוש גם בסביבות תכנות אחרות כמו Python ו-C++. במקום ליצור קובץ זמני, ניתן לעבוד עם משתנים זמניים כדי לשמור על מידע זמני בתוך התוכנית.

## ראו גם:
למידע נוסף על פעולת קובץים זמניים ב-Arduino, מומלץ לעיין במקורות הבאים:
- דוגמאות נוספות ליצירת קבצים זמניים: https://learn.sparkfun.com/tutorials/write-data-to-a-file/
- מידע נוסף על פעולת קבצים ואיך לנהל אותם ב-Arduino: https://www.arduino.cc/en/Tutorial/ReadWrite