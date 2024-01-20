---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

עיבוד תאריך ממחרוזת הוא פעולה שבה אנו ממירים מחרוזת המייצגת תאריך לפורמט של תאריך בתכנות. מתכנתים ביצעים את זה כשעליהם להפוך מידע ממחרוזת - למשל, מקלט ממשתמש - לפורמט נמשך יותר, כמו תאריך.

## איך מבצעים את זה:

דוגמא של קוד:

```Arduino
# include <TimeLib.h>
# include <Wire.h>
# include <DS1307RTC.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) ; 
  setSyncProvider(RTC.get);  
}

void loop() {
  time_t t = now();
  char buffer[] = "DD/MM/YYYY";
  strftime(buffer, sizeof(buffer), "%d/%m/%Y", localtime(&t));
  Serial.println(buffer);
  delay(1000);
}
```

חלונית פלט:

```Arduino
14/02/2022
15/02/2022
16/02/2022
...
```

## הסבר מעמיק

כדי לפענח תאריך ממחרוזת, משתמשים בפונקצית הספרייה `strftime()`, המאפשרת לנו להמיר תאריך בפורמט מחרוזת לממד תאריך ממשי ב-C. אם אתה רוצה לבחון אלטרנטיבות לספריית TimeLib, קיימת ספרייה Arduino שנקראת RTClib שתכלול שירותים דומים. אך שים לב, אף שהפונקציות שונות, התהליך של ניתוח התאריך נשאר זהה.

## ראה גם:

2. [דוקומנטציה של ספריית Arduino RTClib](https://www.arduino.cc/reference/en/libraries/rtclib/)