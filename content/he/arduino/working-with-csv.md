---
title:                "Arduino: עובדים עם CSV"
simple_title:         "עובדים עם CSV"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## למה

CSV הוא פורמט קובץ נפוץ שמשמש לאחסון ושיתוף נתונים בין מכשירים שונים. באמצעות פלטפורמת ארדוינו ניתן לכתוב קוד המאפשר קריאה וכתיבה של נתונים בפורמט CSV, מה שיכול להיות שימושי ביישומים שונים כגון איסוף נתונים מחיישנים או יצירת נתוני מעקב בזמן אמת.

## איך לעשות זאת

כדי לכתוב קוד שיאפשר עבודה עם קבצי CSV בארדוינו, נדרשת פעולה פשוטה בעזרת ספריית סריאל טאטו. הנה דוגמא של קוד פשוט המקרא ומכתב נתונים מקובץ CSV ומדפיס אותם במסך:

```Arduino
#include <SoftwareSerial.h>

SoftwareSerial mySerial(10, 11); // RX, TX pins

void setup() {
  Serial.begin(9600); // initialize serial communication
  mySerial.begin(9600); // initialize SoftwareSerial communication
}

void loop() {
  if (mySerial.available()) {
    String data = mySerial.readStringUntil('\n'); // read data until newline character
    Serial.println(data); // print data to serial monitor
  }
}
```

בכדי לראות את התוכן של הקובץ CSV, ניתן להשתמש בפקודה Serial.print() כדי להדפיס את הנתונים במסך ולעקוב אחריהם בזמן אמת.

## כיוון עמוק

טכניקות נוספות ניתן להשתמש בכדי לעבוד עם נתונים בפורמט CSV בארדוינו. לדוגמא, ניתן להשתמש בספרייה ArduinoCSV לניתוח נתונים מקובץ CSV בצורה מבנית יותר ובספריית TinyCSV מאפשרת ייעודית למכשירים קטנים יותר. כמו כן, ניתן להשתמש בנתונים המצורפים ב-CSV עבור אינטגרציה עם פלטפורמות חיצוניות כגון Excel או Google Sheets.

## ראה גם

- [ArduinoCSV ספרייה](https://github.com/arduino-libraries/ArduinoCSV)
- [TinyCSV ספרייה](https://github.com/adafruit/TinyCSV)
- [פרויקט ארדוינו ריקוז נת