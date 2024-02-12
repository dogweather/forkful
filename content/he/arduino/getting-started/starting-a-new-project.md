---
title:                "התחלת פרויקט חדש"
aliases: - /he/arduino/starting-a-new-project.md
date:                  2024-01-20T18:02:44.497994-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה? (What & Why?)
פתיחת פרויקט חדש בארדואינו זו הקמת תכנית מראשית ליצירת משהו חוויתי או פתרון בעיה. מתכנתים עושים זאת כדי לחקור, ללמוד ולהביא לפועל רעיונות יצירתיים.

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
