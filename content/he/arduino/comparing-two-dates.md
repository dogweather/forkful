---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
שוואת שני תאריכים היא פעולה שמסייעת לנו להבחין בין תאריכים מסוימים. מתכנתים משתמשים בכך לקבלת מידע מפורט יותר וליצור תנאים מותאמים אישית בתוך הקוד שלהם.

## איך לעשות:

הנה דוגמא של שוואת שני תאריכים בארדוינו:

```Arduino
#include <TimeLib.h> 

time_t t1 = now(); // זמן נוכחי
delay(5000); // המתן 5 שניות
time_t t2 = now(); // זמן מעודכן

if(t1 != t2){
  Serial.println("התאריכים שונים");
}else{
  Serial.println("התאריכים זהים");
}
```

דוגמא לפלט של הקוד:

```Arduino
התאריכים שונים
```

## הצצה לתוך הכותרת:

1. תוכן היסטורי: השורת הקוד "TimeLib.h" אינה ייחודית לארדוינו ומשמשת ברבות שפות תכנות אחרות. 
2. חלופות: ניתן לשלב שני מערכות זמן ליצירת מערך זמנים ולהשוות את השורות תוך קיום תנאים מורכבים יותר.
3. פרטי ישום: בספרייה "TimeLib.h", הספרייה משתמשת בפונקציה "now()" לקבלת הזמן הנוכחי.

## עיין גם:

הקישורים הבאים מכילים זמן נוסף כדי לדעת יותר על Arduino ושימושיות שלו עבורנו:
1. [ספרייה TimeLib.h](https://www.pjrc.com/teensy/td_libs_Time.html)
2. [ניתוח דתי יותר עמוק של פונקצית now()](https://arduino.stackexchange.com/questions/12370/time-now-function)