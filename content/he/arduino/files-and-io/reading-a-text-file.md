---
date: 2024-01-20 17:53:55.930773-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: (How to:) ."
lastmod: '2024-03-13T22:44:39.795975-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 22
---

## איך לעשות: (How to:)
```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  myFile = SD.open("test.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Error opening test.txt");
  }
}

void loop() {
  // Nothing here
}
```

פלט לדוגמה:
```
Hello, Arduino! This is a text file.
```

## נפתח לעומק: (Deep Dive)
קריאת קבצי טקסט בארדואינו היא בסיס עבור טעינה ועיבוד נתונים. ההיסטוריה שלה תלויה בתולדות ההתקנים הנשלטים מחשב. בעידן של הארדואינו, זה המרכיב שמאפשר להשיב על שאלות כמו "מה היה גדול המקלחת הלילה?" 

חלופות לקריאת קבצים קיימות, כמו השימוש ב-EEPROM או בחיבור אינטרנט כדי לקרוא נתונים מהענן. בתיאוריה, העקרונות זהים אבל הביצועים נבדלים. אתה פותח קובץ, קורא ממנו בלוקי נתונים ואז עובר עליהם. אמנם יכול להיות שתצטרך לשקול את קיבולת הזיכרון וגודל הקובץ.

## ראה גם: (See Also)
- [המדריך לספריית SD של ארדואינו](https://www.arduino.cc/en/Reference/SD)
- [משאבי EEPROM בארדואינו](https://www.arduino.cc/en/Reference/EEPROM)
- [ממשק SPI בארדואינו](https://www.arduino.cc/en/reference/SPI)
