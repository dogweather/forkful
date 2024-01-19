---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה & למה?
המרת תאריך למחרוזת היא פעולה שבה משנים את מבנה התאריך מאובייקט תאריך לתבנית מחרוזת של בחירתנו. מתכנתים עושים את זה, כדי להוסיף את התאריך לתצוגה למשתמש או לנתוני יומן.

## איך לעשות:
אנו יכולים להמיר תאריך למחרוזת באמצעות המתודה `strftime`. דוגמה:

```Arduino
#include <TimeLib.h>
void setup() {
  time_t now = now();
  char buffer[21];
  strftime(buffer, sizeof(buffer), "%d/%m/%Y %H:%M:%S", localtime(&now));
  Serial.begin(115200);
  Serial.println(buffer);
}
void loop() {
}
```
בדוגמה זו, אנו ממירים את התאריך והשעה הנוכחיים למחרוזת. פלט הקוד:

`01/05/2023 14:30:50`

## צלילה עמוקה
מאז אינטרנט המקורי, מתכנתים ממירים את התאריך למחרוזת כדי להציג למשתמש. זו דרך מקובלת לטפל בתאריכים בצורה שמתכנתים רובם מכירים. החלופות להמרה כוללות את השימוש בטיפוסי מחרוזת אחרים, כולל משתנים ממונה או מערכים.
חלק מהפרטים של הישום: מתודת `strftime` מקבלת שלושה ארגומנטים. הראשון הוא כאן להקצות זיכרון למחרוזת, השני הוא הגודל של המחרוזת, והשלישי הוא תבנית המחרוזת שאנו רוצים להפוך את התאריך לה.

## ראה גם
מאמרים למידה מקוונים אחרים ליצור ולהמיר מחרוזות ב-Arduino:
1. [עיבוד מחרוזות ב-Arduino](https://create.arduino.cc/projecthub/project14/string-manipulation-with-arduino/overview)
2. [המרת תאריכים ושעות](http://playground.arduino.cc/Code/Time)
3. [מזרח קוד Arduino עם TimeLib](https://github.com/PaulStoffregen/Time)