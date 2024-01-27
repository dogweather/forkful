---
title:                "בדיקה האם ספרייה קיימת"
date:                  2024-01-19
html_title:           "Arduino: בדיקה האם ספרייה קיימת"
simple_title:         "בדיקה האם ספרייה קיימת"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם תיקייה קיימת במערכות המבוססות על Arduino היא פרוצדורה שבודקת אם תיקייה מסוימת נמצאת בכרטיס ה-SD או בזיכרון. תכניתנים עושים זאת כדי לוודא שהקבצים והתיקיות שהם רוצים לעבוד איתם ממש קיימים לפני ניסיון לגשת או לשנות אותם, מניעת שגיאות וקריסות.

## איך עושים את זה:
```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("Failed to initialize SD card.");
    return;
  }

  File directory = SD.open("/exampleDir");
  if (directory && directory.isDirectory()) {
    Serial.println("Directory exists!");
  } else {
    Serial.println("Directory not found.");
  }
  directory.close();
}

void loop() {
  // קוד כאן יכול להמשיך לבצע פעילויות אחרות...
}
```
פלט לדוגמה:
```
Directory exists!
```
או
```
Directory not found.
```

## עיון נוסף:
פונקציונליות זו שימושית כבר מאז הוצגו כרטיסי SD בשימוש עם לוחות Arduino. אלטרנטיבות כוללות שימוש בספריות שונות או בפקודות סיסטמה אבל, הספריה SD.h היא שיטת ה-Standard. כאשר אתם בודקים אם תיקייה קיימת, חשוב לסגור אותה עם `directory.close()` כדי לשחרר משאבים. הבדלים באופן שבו מערכות קבצים שונות מנהלות את התיקיות יכולים להשפיע על אופן השימוש בפונקציה בקוד שלכם.

## ראה גם:
- תיעוד SD Library לקבלת מידע נוסף: https://www.arduino.cc/en/Reference/SD
- פורום Arduino לשאלות ותמיכה: https://forum.arduino.cc
- מדריך למערכת הקבצים ב-Arduino: https://www.arduino.cc/en/Guide/Environment#toc9
