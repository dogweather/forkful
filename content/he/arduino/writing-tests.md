---
title:                "כתיבת בדיקות"
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת בדיקות היא יצירת קוד שמוודא שהקוד הראשי שלך עובד כמתוכנן. מתכנתים עושים את זה כדי לזהות באגים מוקדם, לשפר איכות הקוד, ולהקל על תחזוקה בעתיד.

## איך לעשות:

ארדואינו אינו תומך בצורה טבעית ביחידות טסט (unit testing) כמו סביבות פיתוח אחרות. לפיכך, נשתמש בספריה חיצונית כדי להדגים.

```Arduino
#include <ArduinoUnitTests.h>

void setup() {
  Serial.begin(9600);
}

void testLedPin() {
  const int LED_PIN = 13;
  pinMode(LED_PIN, OUTPUT);
  digitalWrite(LED_PIN, HIGH);
  assertEqual(digitalRead(LED_PIN), HIGH);
}

void loop() {
  testLedPin();
  delay(1000);
}
```

פלט לדוגמה:
```
>>> Test Start
>>> Test Passed: testLedPin
>>> Test End
```

## עומק הנושא

בהיסטוריה, כתיבת בדיקות לא תמיד הייתה חלק מהתהליך. אבל, עם הזמן, הקהילה התחילה להעריך את חשיבות הבדיקות בשיפור הקוד. במקרה של ארדואינו, חלק מהאלטרנטיבות כוללות סימולציות ובדיקות פיזיות על הלוח. עדיין, לספריות כמו `ArduinoUnitTests` יש תפקיד חשוב במתן הכלי לבצע בדיקות יחידה.

## גם כדאי לראות

- [Continuous Integration for Arduino](https://learn.adafruit.com/continuous-integration-arduino-and-you) - מדריך לעבודה עם בדיקות אוטומטיות ו-CI בפרויקטי ארדואינו.