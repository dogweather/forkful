---
title:                "קריאת קובץ טקסט"
aliases:
- he/arduino/reading-a-text-file.md
date:                  2024-01-20T17:53:55.930773-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה? (What & Why?)
קריאת קובץ טקסט בארדואינו זה תהליך של גישה לנתונים כתובים מתוך קובץ בכרטיס SD או זיכרון פנימי. מתכנתים עושים זאת כדי לטעון הגדרות, לשמור מידע על ההתקדמות או לעבד דאטה קיים.

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
