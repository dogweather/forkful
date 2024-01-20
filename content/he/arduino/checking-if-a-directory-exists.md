---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Arduino: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה האם תיקייה קיימת היא פעולה בה מנסים לאמת האם נתיב מסוים במערכת הקבצים מתייחס לתיקייה שכבר קיימת. מתכנתים מבצעים פעולה זו כדי למנוע שגיאות כאשר ניסיונות לגשת לתוכן תיקייה שאינה קיימת.

##איך לעשות:
אם אתה משתמש במערכת הפעלה שמריצה קוד Arduino, הנה דוגמה של בדיקת האם תיקייה קיימת:

```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization Failed");
    return;
  }
  if (SD.exists("/dir")) {
    Serial.println("The directory exists.");
  } else {
    Serial.println("The directory doesn't exist.");
  }
}

void loop() {}
```
במקרה שבו התיקייה קיימת, הפלט יהיה:

 ```
The directory exists.
```
ואם התיקייה לא קיימת, הפלט יהיה:
```
The directory doesn't exist.
```

##צלילה עמוקה
תהליך הבדיקה האם תיקייה קיימת הוא חלק מהבסיס של פעילויות מערכת הפעלה. מתכנתים שפיתחו מערכת Arduino אמצו אפשרויות של שפות תכנות נוספות על מנת להנגיש את המערכת למשתמשים. 
אופציה אחרת היא לשפת Python, ב הספרייה "os.path" מספקת פונקציה בשם "exists" שמאפשרת לבדוק אם נתיב מסוים קיים.
ניתן להשתמש בפונקציות מתכנתות תוך כדי שמירה על ראש פתוח לשילוב של שיטות חדשות ומכניזמים נוספים במערכת שלך. 

##ראה גם
- [מדריך חיבור לשרת FTP בשפת Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/FTP)
- [מאמר בנושא השתמש ב-Firebase עם Arduino](https://howtomechatronics.com/tutorials/arduino/how-to-connect-the-arduino-with-google-assistant-and-google-sheets/)
- [מאמר אחר בנושא בדיקת האם תיקייה קיימת בשפת Java](https://www.geeksforgeeks.org/check-if-a-directory-exists-in-java/)