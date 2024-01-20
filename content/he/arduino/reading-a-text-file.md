---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# קריאת קובצי טקסט בארדוינו - המדריך

## מה זה ולמה? (What & Why?)
קריאת קובץ טקסט במהותה אומרת שאנו מנתחים מידע מתוך קובץ טקסט. סבלנותי, זה מאפשר לנו להתמודד עם מגוון מידע, מהדטה סטרימינג של דֵיְטאָה ועד להגדרות של האפליקציה.

## כיצד: (How to:)
הרוב המוחלט של המשאית הוא דוּגְמָה של קוד לקריאת קובץ מסוים. הוא משתמש בספריה `SD` בשפת תכנות ארדוינו.


```Arduino
#include <SD.h>

// הגדרת חיבור לכרטיס אחסון
#define chipSelect 4

void setup()
{
  Serial.begin(9600);
  // בדיקה אם הכרטיס מחובר
  if (!SD.begin(chipSelect)) {
    Serial.println("חיבור לכרטיס נכשל");
    return;
  }
}

void loop()
{
  File dataFile = SD.open("test.txt");

  // אם הקובץ נפתח בהצלחה, הדפסת התוכן
  if (dataFile) {
    while (dataFile.available()) {
      Serial.write(dataFile.read());
    }
    dataFile.close();
  }

  else {
    Serial.println("אי אפשר לפתוח את test.txt");
  }

  // מנחה של 5 שניות
  delay(5000);
}
```

## עומק הדעת (Deep Dive)
קריאת קובץ מאפשרת לנו להתמודד עם מידע נרחב. היא מוחלטת מאז התקופה של המחשב האישי. כיום, אנחנו משתמשים בה רוב הזמן כאשר אנחנו מנתחים קובץ.

אם אנחנו מסתכלים על חלופות, אנו יכולים לדבר על שימוש בבסיסי נתונים, ניתוח פורטוקולים מסוימים להעברת נתונים, ועוד. אבל תמיד נתקלים במושג של "קריאת קובצי טקסט".

## קישורים רלוונטיים (See Also)
1. [מערכת קבצים SD](https://www.arduino.cc/en/reference/SD)
3. [ניתוח YAML וקובצים JSON](https://arduinojson.org/)