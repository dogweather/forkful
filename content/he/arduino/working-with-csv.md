---
title:                "עבודה עם קבצי CSV"
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם קבצי CSV מאפשרת לאחסן ולנהל מידע בפורמט טקסט פשוט שמופרד בפסיקים. תוכנאים משתמשים בכך כדי לשמור נתונים ממכשירי חיישנים או לייבא/ייצא מידע לשימושים אחרים.

## איך לעשות:
```
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  SD.begin(4);
  myFile = SD.open("data.csv", FILE_WRITE);

  if (myFile) {
    myFile.println("sensor1,sensor2");
    myFile.println(String(analogRead(A0)) + "," + String(analogRead(A1)));
    myFile.close();
  } else {
    Serial.println("Error writing to file");
  }
}

void loop() {
  // Nothing here for now
}
```
פלט לדוגמה: בקובץ `data.csv` יופיעו שורות של קריאות חיישן.

## עיון מעמיק:
CSV מקורו בשנות ה-70 ונועד לייצוג נתונים טבלאיים פשוט. חלופות כוללות פורמטים כמו JSON או XML, אך CSV נשאר מבנה פופולרי בזכות פשטותו. בעבודה עם ארדואינו, חשוב להתחשב במגבלות זיכרון ולהימנע משימוש בספריות כבדות.

## ראה גם:
- [תיעוד הספרייה SD של ארדואינו](https://www.arduino.cc/en/Reference/SD)
- [הספרייה SPI של ארדואינו](https://www.arduino.cc/en/Reference/SPI)
- [מדריך לקריאה וכתיבה של קבצי CSV בפייתון](https://realpython.com/python-csv/) (אם רוצים להרחיב ולעבד את הנתונים על מחשב)
