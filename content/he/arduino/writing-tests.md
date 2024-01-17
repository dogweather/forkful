---
title:                "כתיבת מבחנים"
html_title:           "Arduino: כתיבת מבחנים"
simple_title:         "כתיבת מבחנים"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת בדיקות היא תהליך חשוב בכתיבת קוד, שבו אנו מציגים תרגילי בדיקה לקוד שנכתב כדי לוודא שהוא פועל כפי שצריך. הבדיקות חיוניות כי הן מאפשרות לנו לגלות באופן מוקדם מאוד כאשר יש תקלות בקוד ולתקן אותם, מכיוון שניתן להכניס אותם לתוך הקוד את התנאים הנכונים.

## איך לעשות זאת?

הנה דוגמה של בדיקה פשוטה שניתן לממש באמצעות קוד Arduino:

```
Arduino.setup() {
  chackInput();
  if (!input) {
    print("No input detected");
  }
  else {
    print("Input detected");
  }
}
```

 פלט צפוי:
```
No input detected
```

כאן אנו בודקים אם יש קלט כניסה מכיוון שזה חיוני לתכנית שלנו. אם אין קלט ניתן להשתמש בפקודה "print" על מנת להציג הודעה מתאימה. אחרת, ניתן להמשיך לבנות את התכנית שלנו.

## חפירה עמוקה

בדיקות היו חשובות כי הן מסייעות לצבור ידע על תקלות בקוד ולתקן אותן, כך שאפשר לשפר את המוצרים שאנו מפתחים. יש גם אפשרות לבצע חלופות לבדיקות באמצעות כלי צד שלישי, כגון JUnit עבור קוד Java ו-NUnit עבור קוד .NET. בנוסף, כיוון שקוד ארדואינו מבנה בודד, אפשר להשתמש ביכולות פונקציות מתוך תכניות קוד אחרות כדי לעזור בבדיקות התכנית.

## ראה גם

למידע נוסף על בדיקות קוד עבור ארדוינו, ראה:
- [Arduino Testing Tutorial](https://www.arduino.cc/en/Guide/ArduinoTestingTutorial)
- [Arduino Unit Testing Library](https://github.com/mmurdoch/arduinounit)
- [How to Test Arduino Code by Writing Unit Tests](https://www.theengineeringprojects.com/2018/08/how-to-test-arduino-code-by-writing-unit-tests.html)