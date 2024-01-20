---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

יצירת קובץ זמני היא פעולה שבה מכינים קובץ שנמחק בתום השימוש. מתכנתים משתמשים בקבצים זמניים כאשר הם רוצים לשמור מידע מעבר בלתי קבוע, ללא צורך בשמירתו לאורך זמן.

## איך לעשות:

הנה דוגמה ליצירת קובץ זמני באמצעות קוד Arduino:

```Arduino
#include <SD.h>

File tempFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    while (1);
  }

  tempFile = SD.open("temp.txt", FILE_WRITE);
  if (!tempFile) {
    Serial.println("File creation failed!");
    while (1);
  }
  Serial.println("File creation successful!");
}

void loop() {
  // write to the file
  tempFile.println("Temporary file data.");
  tempFile.close();  // close the file
  
  // delete the file
  if (SD.remove("temp.txt")) {
    Serial.println("File deletion successful!");
  } else {
    Serial.println("File deletion failed!");
  }
}
```

פלט דוגמה:

```
File creation successful!
File deletion successful!
```

## צלילה עמוקה:

1) היסטוריה: במערכות ההפעלה המודרניות, קבצים זמניים יכולים להיות נוצרים בקלות לצרכים של האפליקציה. זה מאפשר למתכנתים להשתמש בקבצים זמניים לצורך ריבוי פעולות.

2) אלטרנטיבות: לחיסכון במקום ניתן לשמור מידע מעבר בזיכרון ה-RAM, אך לכך יש את החסרון בחשיף של המידע לאובדן במידה וקורה התרעה כלשהי.

3) פרטי המימוש: כאשר אנו מוחקים קובץ מה-SD, המחיצה מסמנת את המקום שעליו הקובץ היה כשלילי לשימוש חוזר.

## ראה גם:

1) [תיעוד המחלקה File של SD ב- Arduino](https://www.arduino.cc/en/Reference/SD)
2) [הסבר מפורט על שימוש בקבצים ב- Arduino](https://www.dummies.com/programming/arduino/how-to-save-a-file-to-an-sd-card-with-arduino/)
3) [פורום Arduino עם דיונים ושאלות נפוצות](https://community.arduino.cc/)