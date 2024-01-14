---
title:                "Arduino: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מדוע:

ישנם מקרים בהם מספר תהליכים עוברים על דאטה והוא צריך להישמר לפני שניתן להשתמש בו לצורךים אחרים. כתיבת קוד זמני הוא כלי שימושי במיוחד כאשר אתה משתמש במכשיר Arduino, שומר את המידע שלך לפני שהוא מתעדכן לתהליך או מגיע לצורך יישום אחר.

## כיצד לעשות:

דוגמאות קוד ופלט המתבצעות רשאים יהיו הדרך האידיאלית להדגים את כיצד ליצור קובץ זמני בארדואינו. כאשר דרך זו עובדת היטב, הטכניקה המתקדמת יותר או שוספרים נוספים על ידי שטיפת נפש היא קלה יותר להבין.

```arduino
#include <SD.h>
File myFile;

void setup() {
  Serial.begin(9600);
  while (!Serial) {}
  pinMode(10, OUTPUT);

  Serial.print("Initializing SD card...");
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  Serial.println("Initialization done.");
  
  // create a temporary file on the SD
  File myFile = SD.open("temp.txt", FILE_WRITE);
  if (myFile) {
    Serial.println("Writing to temporary file...");
    myFile.println("Hello from Arduino!");
    myFile.close();
    Serial.println("Temporary file created.");
  } else {
    Serial.println("Error opening temporary file.");
  }
}

void loop() {

}
```

## חפוש עמוק:

ישנם מספר דברים נוספים שאפשר לעזור אתכם ליצור קובץ זמני בארדואינו. למשל, ניתן להשתמש בספרייה SD כדי להתחבר לכרטיס SD ולממש כל פעולת קובץ שאתם נדרשים. ישנם גם טיפים נוספים על כיצד לנהל קבצים בארדואינו ולהשתמש בהם בצורה כמו קבצי פתח פתח ובסופה בבניית קובץ ידנית. אל תתאבדו אם ישנם עוד תלותם בפעולות על קובץ זמני בארדואינו.

## ראו גם:

- [ספריית SD של ארדואינו](https://www.arduino.cc/reference/en/libraries/sd/)
- [כתבו וקראו קבצים עם Arduino](https://www.instructables.com/