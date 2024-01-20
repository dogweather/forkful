---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
דפוס של פלט לניפוי תקלות (Debug Output) הוא טכניקה שבה מתכנתים משתמשים לפצל, לנתח ולבחון את הקוד שלהם. מתכנתים מדפיסים את הפלט הזה כדי לראות את מהלך ההרצה של הקוד, לזהות בעיות ולגבש פתרונות.

## איך לעשות: 
בארדואינו, אפשר להשתמש בפונקציה `Serial.print()` לדפוס פלט לניפוי תקלות.
```
Arduino
void setup() {
  Serial.begin(9600); // הגדרת מהירות הקשר
}

void loop() {
  Serial.println("זהו מקטע קוד הדפסה לניפוי");  // הדפסת יומן ניפוי
  delay(1000);  // השהייה אינטרוול של שנייה
}
```
פלט לדוגמה: `זהו מקטע קוד הדפסה לניפוי`.

## צלילה עמוקה: 
הטכניקה של דפוס פלט לניפוי נפותה בתחילת שנות ה-70 ככלי חיוני לניפוי תקלות. דרך חלופית לדפוס פלט לניפוי היא באמצעות יומנים (logging), אולם בארדואינו, פונקציה Serial.print() היא הנפוצה ביותר. מנגנון הדפוס משתמש בפורט טרמינל שסידורי (Serial) לשידור מידע אל המחשב לניפוי תקלות.

## ראו גם: 
1. [תיעוד רשמי של Arduino על הפונקציה `Serial.print()`](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
2. [מדריך אינטריודי לדפוס פלט לניפוי בארדואינו](https://learn.sparkfun.com/tutorials/serial-communication/all)
3. [מדריך ויקי על הדפסה לניפוי תקלות](https://en.wikipedia.org/wiki/Debugging#Print_debugging)