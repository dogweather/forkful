---
title:    "Arduino: כתיבת קובץ טקסט"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה

עכשיו שאנחנו יודעים כיצד להתקנן ולהשתמש ב- Arduino, אנחנו רוצים לדעת איך להשתמש בו כדי לכתוב קבצי טקסט. כתיבת קוד נייד יכולה להיות מאתגרת, אבל היא יכולה גם להיות מאיימת כששומרים את האמונה של הודעות לאחרים. עם Arduino, אנחנו יכולים לכתוב קבצי טקסט בקלות, תוך שימוש בפעולות פשוטות ובמשאבי החומרה המובנים של הודעות החיבורים החומרה.

## כיצד

```arduino
// ייבוא ספריות הכרחיות
#include <SPI.h>
#include <SD.h>

File textFile; // הצהרה על אובייקט קובץ טקסט

void setup() {
  // התחלת היצירה של קשר הרשת
  Serial.begin(9600);
  while (!Serial) {
    ; // המתנה עד שיתחבר פלט אוניברסלית ליישום כדי להתחיל לנסח הודעה
  }

  // להפעיל גישה לכרטיס SD
  if(!SD.begin(4)) {
    Serial.println("נמצאה שגיאה - אינך יכול לכתוב קבצי טקסט ערים");
    return;
  }

  // נפתח או יוצרת קובץ, אם קובץ כבר שקיפי
  textFile = SD.open("הודעות.txt", FILE_WRITE);

  // ייצור תאריך את תאריך הדפדף של הודעות
  while (textFile.println('הודעת דוגמא') == 0) {
    Serial.println("הגדרת תאריך הדפדפן של הודעות נכשלה");
    // בחוזר היכולת לשבת ולנסות שוב כל הפעם תאריך מסוים
  }
}

void loop() {
  // קריאה סדרתית של הודעות וכתירגל לשלום
  // מודפס את כל זה דרך של הודעות הגריל
  if ( textFile.available() ) {
    Serial.write( textFile.read() );
  }

  // את כיתובות אחת על זה
  if ( Serial.available() ) {
    textFile.write( Serial.read() );
  }
}

```
> דגימה: הודעת דוגמא

אפשר לבחור את התאריך את או לייצר אובי