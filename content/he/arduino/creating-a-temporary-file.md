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

## למה
על מה שאדם יאתגר ליצור קובץ זמני?

ישנם כמה סיבות שאדם עשוי לבחור ליצור קובץ זמני בארדוינו. עשיריית השנות ואמונת נהנים מגרסה ישנה של בשנים רבות ואת ארבעת הטבעות המדוברים על ידי השמה לטווח קצוב יותר. כשאתה ממתין למידע שאתה מופץ באותה דרך כמו כול גרסה כוללת.

## איך לה
בכדי ליצור קובץ זמני בארדוינו, תצטרכו רק כמה שורות קוד פשוטות. הנה דוגמה לכתיבת קובץ זמני והדפסת הודעה:

```Arduino
#include <SPI.h>
#include <SD.h>

File dataFile;

void setup() {
  // התחבר למתאם SD
  Serial.begin(9600);
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB
  }

  // התחבר לכרטיס SD
  if (!SD.begin(4)) {
    Serial.println("לא הצליח לטעון כרטיס SD");
    while (1);
  }

  // פתח קובץ זמני עם השם מזימון
  dataFile = SD.open("מזימון.txt", FILE_WRITE);

  // אם הקובץ נפתח בהצלחה, יוסף הודעה
  if (dataFile) {
    dataFile.println("היי, זהו קובץ זמני!");
    dataFile.close();
    Serial.println("קובץ זמני נוצר בהצלחה.");
  } else {
    // אם הקובץ לא נפתח, הדפס הודעה שגיאה
    Serial.println("שגיאה בפתיחת קובץ זמני.");
  }
}
```

פלט:

```
קובץ זמני נוצר בהצלחה.
```

כפי שאתה רואה, כתיבת קובץ זמני בארדוינו היא תהליך פשוט וקל.

## "לשוט"

ליצור קובץ זמני הוא פעולה שניתן לבצע באמצעות כמה שורות קוד בלבד, אבל כדאי להבין את העקרונות בסיסיים של יצירת קובץ זמני. הקובץ זמני הוא קובץ שנוצר בזמן הרצת הקוד ונמחק בסיומו, ו