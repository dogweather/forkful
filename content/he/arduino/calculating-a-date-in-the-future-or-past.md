---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Arduino: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא פעולה שמאפשרת לנו להגיע לתאריך מסוים המבוסס על מרחק זמן מוחלט מתאריך התחלתי. זה שימושי בתכנות כאשר אנו צריכים להגביל או לתזמן אירועים לפי תאריכים מסוימים.

##  איך מבצעים:
הנה דוגמא של קוד שמראה איך לחשב תאריך בעתיד או בעבר בארדוינו:

```Arduino
#include <TimeLib.h>
tmElements_t tm;

void setup() {
  setTime(8, 29, 0, 1, 1, 2021); // Set initial date and time
}

void loop() {
  if (now() % 2 == 0) {
    addFutureDate(5); // Add 5 days to current date
  } else {
    addPastDate(3); // Subtract 3 days from current date
  }
}

void addFutureDate(int daysToAdd) {
  breakTime(now() + daysToAdd * SECS_PER_DAY, tm);
  printDate();
}

void addPastDate(int daysToSubtract) {
  breakTime(now() - daysToSubtract * SECS_PER_DAY, tm);
  printDate();
}

void printDate() {
  Serial.print(tm.Day);
  Serial.print("/");
  Serial.print(tm.Month);
  Serial.println("/");
  Serial.println(tm.Year);
}
```

## צלילה עמוקה
חישוב תאריך בעתיד או בעבר היה חלק מתכנות מאז התחלתו. תוך שהזמן התפתח, ישנן אפשרויות חלופיות רבות שכוללות שימוש בסיפריות חיצוניות או שימוש בפונקציות מובנות. בארדוינו, סיפריית TimeLib מספקת דרך פשוטה וישירה לעבוד עם תאריכים וזמנים.

## ראה גם
[TimeLib Library Documentation](https://www.arduino.cc/en/Reference/Time)
[Arduino Time Library](https://playground.arduino.cc/Code/Time/)