---
title:                "טיפול בשגיאות"
date:                  2024-01-26T00:51:06.325610-07:00
model:                 gpt-4-1106-preview
simple_title:         "טיפול בשגיאות"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/handling-errors.md"
---

{{< edit_this_page >}}

## מה ולמה?

טיפול בשגיאות בתוכניות שלך תופס את הדברים שלא צפית להם שינסו להפיל אותך. אתה עושה זאת כדי לשמור על ה-Arduino שלך מתפרצת לחץ כאשר הבלתי צפוי מתרחש.

## איך לעשות:

נניח שה-Arduino שלך קורא מחיישן שלעיתים עלול לייצר ערכים מחוץ לטווח. הנה איך כדאי לך להתמודד עם זה:

```Arduino
int sensorValue = analogRead(A0);

if (sensorValue >= 0 && sensorValue <= 1023) {
  // הערך נמצא בטווח, המשך עם העיבוד
  Serial.println(sensorValue);
} else {
  // הערך מחוץ לטווח, טפל בשגיאה
  Serial.println("Error: Sensor value out of range.");
}
```
פלט לדוגמה:
```
523
Error: Sensor value out of range.
761
```

## צלילה עמוקה

טיפול בשגיאות לא תמיד היה כל כך פשוט. בימים הראשונים, מפתחים לעיתים קרובות התעלמו משגיאות, מה שהוביל ל"התנהגות בלתי מוגדרת" שכולם פחדו ממנה. ככל שתכנות התפתח, כך גם הכלים - כעת יש לך חריגות בשפות רבות, אבל הן עדיין 'בדוק קודם' באופן ממשי בעולם ה-Arduino בשל אילוצי החומרה והשורשים ב-C++.

בתכנות Arduino, לעיתים קרובות תראה הוראות `if-else` לטיפול בשגיאות. אך ישנם חלופות: שימוש בפונקציה `assert` לעצירת הביצוע אם תנאי כלשהו נכשל או תכנון מנגנוני נכשול בתוך ההגדרה של החומרה עצמה.

כאשר אתה מיישם טיפול בשגיאות, שקול את השפעת העצירה של התוכנית לעומת האפשרות להמשיך אותה עם מצב ברירת מחדל או מצב בטוח. יש פשרה, והבחירה הנכונה תלויה בנזק האפשרי מהפרעות לעומת פעולה לא נכונה.

## ראה גם

התעמק בזיהוי שגיאות וטיפול בהן עם אלה:

- מראה על שפת Arduino: https://www.arduino.cc/reference/en/
- מבט עמוק יותר של Embedded Artistry על טיפול בשגיאות: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- טיפול בשגיאות ב-C++: https://en.cppreference.com/w/cpp/error/exception

זה אמור לתת לך את הידע והביטחון להימנע מהמלכודות של שגיאות ברפת של חוויות ה-Arduino שלך.
