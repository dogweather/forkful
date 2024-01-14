---
title:                "Arduino: לקבלת תאריך נוכחי - קבלת התאריך הנוכחי"
simple_title:         "לקבלת תאריך נוכחי - קבלת התאריך הנוכחי"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# למה
ידע תאריך נוכחי יכול להיות מאוד שימושי בפרויקט Arduino שלכם. הוא מאפשר לכם ליצור תכניות שימושיות שתוכלו להשתמש בהן בהתאם לתאריך, כגון מערכות לשעות או ביקורות יומיות.

# איך לעשות זאת
ניתן לקבל את התאריך הנוכחי בקלות באמצעות מכשיר RTC (Real-Time Clock). נתחיל עם הכנה קוד ב Arduino IDE:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS1307 rtc; // יצירת אובייקט RTC עבור RTC DS1307

void setup () {
  Serial.begin(9600); // התחלת חיבור למסך מוניטור סידרתי
  rtc.begin(); // התחלת חיבור לשעון RTC
}

void loop () {
  DateTime now = rtc.now(); // קבלת תאריך ושעה נוכחיים ממכשיר RTC
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.print(' ');
  Serial.print(now.hour(), DEC);
  Serial.print(':');
  Serial.print(now.minute(), DEC);
  Serial.print(':');
  Serial.print(now.second(), DEC);
  Serial.println();
}
```

כאשר אתם מעתיקים ומדביקים את הקוד ל Arduino IDE שלכם, לא לשכוח להוסיף את הספריות הנדרשות לקובץ הגלאבליינסטון.

אחרי שתקמלו ותעלו את הקוד למכשיר שלכם, תוכלו לראות במסך המוניטור סידרתי את התאריך והשעה הנוכחים.

# טיפול מעמיק
לקבלת תאריך ושעה אינם מספיקים רק לצורך הצגה על מסך המוניטור. אתם יכולים להשתמש בתאריכים הנוכחיים לביצוע פעולות אחרות כמו קבלת זמן פתיחת וסגירת דלתות או נתינת אתר תפוקת התמונה של מצלמה. כדי לעשות זאת, יש להשתמש בספריית DS1307RTC כדי לקבל את ההרכבים הנכונים מתוך התאריך והשעה הנוכחיים ולהשתמש באחדים מהם בקוד שלכם.

זוהי פרטי