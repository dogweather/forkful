---
date: 2024-01-20 18:02:44.497994-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: (How to:) \u05D4\u05E0\
  \u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4 \u05E9\u05DC\
  \ \u05E7\u05D5\u05D3 \u05D4\u05D3\u05D5\u05DC\u05E7 \u05D5\u05DB\u05D9\u05D1\u05D4\
  \ LED."
lastmod: '2024-03-13T22:44:39.768983-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4 \u05E4\u05E9\u05D5\u05D8\
  \u05D4 \u05E9\u05DC \u05E7\u05D5\u05D3 \u05D4\u05D3\u05D5\u05DC\u05E7 \u05D5\u05DB\
  \u05D9\u05D1\u05D4 LED."
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
weight: 1
---

## איך לעשות: (How to:)
הנה דוגמה פשוטה של קוד הדולק וכיבה LED:

```Arduino
void setup() {
  pinMode(13, OUTPUT); // הגדרת פין 13 כיציאה
}

void loop() {
  digitalWrite(13, HIGH);   // הדלקת ה-LED
  delay(1000);              // המתנה של שנייה
  digitalWrite(13, LOW);    // כיבוי ה-LED
  delay(1000);              // המתנה שוב לפני החזרה על הקוד
}
```
כאשר תעלה את הקוד ללוח, ה-LED שמחובר לפין 13 יידלק ויכבה במרווחי זמן של שנייה.

## צלילה לעומק: (Deep Dive)
ה-Arduino יצא לאור ב-2005 ומאז נהפך לכלי פופולרי בקרב חובבים ומתכנתים כאחד. בעוד שיש חלופות כמו Raspberry Pi או micro:bit, ה-Arduino נשאר פופולרי בזכות הפשטות והגמישות שלו. כאשר פותחים פרויקט חדש, חשוב להכיר את הלוח שבו אתה עובד, המאפיינים שלו והאיך שהוא מחובר לחומרה.

## ראה גם: (See Also)
- [תיעוד Arduino](https://www.arduino.cc/reference/en/)
- [מדריך למתחילים ב-Arduino](https://www.arduino.cc/en/Guide/HomePage)
- [מיזמי Arduino למתחילים](https://create.arduino.cc/projecthub?by=beginners)
