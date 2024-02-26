---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:34.284245-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9\
  \ CSV (\u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD\
  \ \u05D1\u05E4\u05E1\u05D9\u05E7\u05D9\u05DD) \u05D1-Arduino \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\u05E7\u05D1\u05E6\u05D9 CSV \u05D5\u05DB\
  \u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\u05D4\u05DD, \u05E9\u05D1\u05D3\u05E8\
  \u05DA \u05DB\u05DC\u05DC \u05DE\u05D0\u05D5\u05D7\u05E1\u05E0\u05D9\u05DD \u05E2\
  \u05DC \u05DB\u05E8\u05D8\u05D9\u05E1 SD, \u05DE\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\
  \u05E8 \u05E8\u05D9\u05E9\u05D5\u05DD \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D4\
  \u05D2\u05D3\u05E8\u05D5\u05EA\u2026"
lastmod: '2024-02-25T18:49:38.043288-07:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 CSV\
  \ (\u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\
  \u05E4\u05E1\u05D9\u05E7\u05D9\u05DD) \u05D1-Arduino \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\u05E7\u05D1\u05E6\u05D9 CSV \u05D5\u05DB\
  \u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\u05D4\u05DD, \u05E9\u05D1\u05D3\u05E8\
  \u05DA \u05DB\u05DC\u05DC \u05DE\u05D0\u05D5\u05D7\u05E1\u05E0\u05D9\u05DD \u05E2\
  \u05DC \u05DB\u05E8\u05D8\u05D9\u05E1 SD, \u05DE\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\
  \u05E8 \u05E8\u05D9\u05E9\u05D5\u05DD \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D4\
  \u05D2\u05D3\u05E8\u05D5\u05EA\u2026"
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם קבצי CSV (ערכים מופרדים בפסיקים) ב-Arduino כוללת קריאה מקבצי CSV וכתיבה אליהם, שבדרך כלל מאוחסנים על כרטיס SD, מה שמאפשר רישום נתונים, הגדרות תצורה ועוד. מתכנתים לעיתים קרובות מתמודדים עם קבצי CSV עבור איסוף נתוני חיישנים, אחסון פרמטרים של תצורה, או ממשק עם מערכות אחרות, בשל פשטותם והתקבלותם הרחבה בפלטפורמות שונות.

## איך לעשות זאת:
ל-Arduino אין ספרייה מובנית במיוחד לטיפול בקבצי CSV, אבל ניתן להשתמש בספריות `SD` ו-`SPI` לגישה לקבצים על כרטיס SD, ולאחר מכן לנתח או לייצר נתוני CSV באמצעות טכניקות פשוטות של עיבוד מחרוזות. כאשר מתמודדים עם מניפולציות CSV מורכבות יותר, ניתן להשתמש בספרייה של צד שלישי `ArduinoCSV` עבור ניתוח וכתיבה קלים יותר.

**קריאת נתוני CSV מכרטיס SD:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // מדפיס את שורת ה-CSV
    }
    dataFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // לא בשימוש בדוגמה זו
}
```
*דוגמת פלט:*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**כתיבת נתוני CSV לכרטיס SD:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Value"); // כותרת CSV
    dataFile.println("1, 1597840923, 23.5"); // שורת נתונים לדוגמה
    dataFile.close();
    Serial.println("Data written");
  } else {
    Serial.println("Error opening output.csv");
  }
}

void loop() {
  // לא בשימוש בדוגמה זו
}
```
*דוגמת פלט:*
```
Data written
```

**שימוש ב-ArduinoCSV לניתוח:**
אם עוסקים בקבצי CSV מורכבים, הספרייה `ArduinoCSV` יכולה לפשט מאוד את ניסיונות הניתוח. הדוגמה הזו מניחה שכבר התקנתם את ספריית `ArduinoCSV`.

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // מדפיס כל שדה
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // לא בשימוש בדוגמה זו
}
```
*דוגמת פלט:*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
בדוגמאות אלה, על ידי קריאה מקבצי CSV וכתיבה אליהם על כרטיס SD, פרויקטים של Arduino יכולים בקלות לאסוף נתונים, לאחסן הגדרות תצורה, או להחליף נתונים עם יישומים אחרים בפורמט נגיש באופן גלובלי.
