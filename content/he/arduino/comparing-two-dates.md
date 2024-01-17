---
title:                "השוואת שתי תאריכים"
html_title:           "Arduino: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
להשוות שני תאריכים היא פעולה חשובה בתכנות של Arduino. מתכנתים משתמשים בזה כדי לבדוק אם תאריך מסוים הוא לפני או אחרי תאריך אחר, או לצורך בדיקת תאריך מסוים בזמן הריצה של תוכנית.

## איך לעשות:
הקוד הבא מדגים כיצד לבצע השוואה בין שני תאריכים בעזרת Arduino:
```Arduino
#include <TimeLib.h>

void setup() {
  // נגדיר שני תאריכים להשוואה
  time_t date1 = makeTime(0, 30, 15, 7, 6, 2019); // 15:30 יום שני, ה-7 ליוני 2019
  time_t date2 = makeTime(13, 45, 22, 7, 6, 2019); // 13:45 יום שני, ה-22 ליוני 2019

  // נגדיר את הפונקציה בעצמנו לביצוע ההשוואה
  bool isDate1BeforeDate2 = compareDates(date1, date2);

  // נדפיס את תוצאת ההשוואה למסך
  if (isDate1BeforeDate2) {
    Serial.println("תאריך 1 הוא לפני תאריך 2");
  } else {
    Serial.println("תאריך 1 הוא אחרי או שווה לתאריך 2");
  }
}

void loop() {
  // לא נעשה שום דבר כי הפעולה מתבצעת רק בהתחלה
}

// פונקציה להשוואת שני תאריכים
bool compareDates(time_t d1, time_t d2) {
  if (year(d1) < year(d2)) {
    // השנה של תאריך 1 היא לפני השנה של תאריך 2
    return true;
  } else if (year(d1) == year(d2)) {
    if (month(d1) < month(d2)) {
      // בשנת זהה תאריך 1 הוא לפני תאריך 2
      return true;
    } else if (month(d1) == month(d2)) {
      if (day(d1) < day(d2)) {
        // בחודש זהה תאריך 1 הוא לפני תאריך 2
        return true;
      } else if (day(d1) == day(d2)) {
        if (hour(d1) < hour(d2)) {
          // ביום זהה תאריך 1 הוא לפני תאריך 2
          return true;
        } else if (hour(d1) == hour(d2)) {
          if (minute(d1) < minute(d2)) {
            // בשעה זו תאריך 1 הוא לפני תאריך 2
            return true;
          }
          // שעה ודקות זהות, לכן תאריך אחד אינו לפני תאריך אחר
          return false;
        }
        // יום ושעה זהות, לכן תאריך אחד אינו לפני תאריך אחר
        return false;
      }
      // חודש ויום זהות, לכן תאריך אחד אינו לפני תאריך אחר
      return false;
    }
    // שנה וחודש זהות, לכן תאריך אחד אינו לפני תאריך אחר
    return false;
  }
  // השנה של תאריך 1 היא אחרי השנה של תאריך 2
  return false;
}
```

פלט למסך יהיה:
```
תאריך 1 הוא לפני תאריך 2
```

## חפירה עמוקה:
כשהשוואת תאריכים הופכת לקשה ומסורבלת, ניתן להשתמש בספרייה חיצונית כגון [TimeLib](https://www.arduino.cc/en/Reference/Time), אשר כוללת פונקציות מובנות לפעולות עם תאריכים. בנוסף, ניתן להשוות תאריכים לפי כמה מספראות:

- השוואה של תאריך מסוים ע