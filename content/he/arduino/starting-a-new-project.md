---
title:                "התחלת פרויקט חדש"
html_title:           "Clojure: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
התחלת פרויקט חדש ב-Arduino היא מסבך של ציוד והתקנת התכנים הנדרשים ליצירת אפליקציה שניתן להריץ בטיולים. תהליך זה רלוונטי למתכנתים מכיוון שזה מאפשר להם להדגים את יכולותם ולפתח את מערך הידע שלהם בפלטפורמה בעלת כוח מעשיי רב.

## איך ל:
כאן הקוד שלך והתוצאה שלו ישארו בתוך מסגרת ```Arduino ... ```.

```Arduino
// משתנה לנורת LED
int LED = 13;

// הגדרת המשתנה כפלט
void setup () {
 pinMode (LED, OUTPUT);
}

// הדלקה וכיבוי של הנורה
void loop () {
 digitalWrite (LED, HIGH);
 delay (1000);
 digitalWrite (LED, LOW);
 delay (1000);
}
```

הקוד הזה ידליק נורת LED לאורך זמן של שנייה, ואז יכבה אותה לאורך זמן של שנייה. 

## צלילה עמוקה:
התחלת פרויקט חדשה בArduino היא מושג שנמצא בשימוש מאז השנות ה-1980, כאשר השפה עצמה נוצרה. חלופות כוללות Raspberry Pi, BeagleBone, וכד,' אם כי Arduino הוא פופולרי במיוחד בשל העשרה של ביבליות שנגישות וקהילת המתכנתים הנרחבת שלה. על מנת להתחיל פרויקט חדש נדרשת התקנה של Arduino IDE.

## ראה גם:
[Arduino IDE download](https://www.arduino.cc/en/Main/Software)
[Starting with Arduino](https://www.arduino.cc/en/Guide/HomePage)
[Arduino programming basics](https://www.arduino.cc/en/Tutorial/Foundations)