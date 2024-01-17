---
title:                "להתחיל פרוייקט חדש"
html_title:           "Arduino: להתחיל פרוייקט חדש"
simple_title:         "להתחיל פרוייקט חדש"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?
להתחיל פרויקט חדש הינו התהליך בו מתחילים לבנות את התוכנית שלכם בארדוינו. תהליך זה חשוב כיוון שהוא מאפשר לכם ליצור מוצר סופי שיכול לשמש לפיתוח רעיונות ולמימוש פרויקטים שונים.

## איך לעשות זאת:
להלן מספר דוגמאות של קוד ופלט תוך שימוש במבנה ```Arduino ... ```.

### מבנה תכנות מקור לשימוש בקלט ובפלט
```arduino
//קבלת נתונים מהכנס
int inputPin = A0;
int inputValue;

void setup() {
  //להגדיר את הכנס ככנס דיגיטלי
  pinMode(inputPin, INPUT);
  //להגדיר את הכנס ככנס אנלוגי
  pinMode(inputPin, INPUT);
  //להדליק את המפלצת
  Serial.begin(9600);
}

void loop() {
  //קריאת נתונים מהכנס דיגיטלי
  inputValue = digitalRead(inputPin);
  Serial.println(inputValue);
}
```

### ניהול מצבים ותנאים באמצעות יישומת IF
```arduino
//הגדרת ערך לאינפוט ולאופוט
int inputPin = A0;
int inputValue;

//הגדרת משתנה למצב
int state = 0;

void setup() {
  //הכנס כניסה כתוכנית אנלוגית
  pinMode(inputPin, INPUT);
  //הדלקת מפלצת
  Serial.begin(9600);
}

void loop() {
  //קבלת נתון מהכניסה האנלוגית
  inputValue = analogRead(inputPin);

  //בדיקה אם הערך האנלוגי גדול מ-500
  if (inputValue > 500) {
    state = 1;
  }
  //בהפך
  else {
    state = 0;
  }
  Serial.println(state);
}
```

## חקירה מעמיקה:
תהליך זה התחיל את מפותחו של Massimo Banzi, ויצא לכלל כמחברת פרופיל לפיתוח פשוט ומקפץ של זמן קצר ועורך קוד עם יכולות אנלוגיות / דיגיטליות כגון מעקב קפסולה, ובעתיד סוללות. ישנם כמה כלים ותוכניות זמינות כדי לעזור לך להתחיל פרויקטים חדשים בארדוינו עם כל מכשיר הקשה או התוכנה מגוונים.

## ראה גם:
למידת ארדוינו: https://www.arduino.cc/en/Guide/HomePage