---
title:                "מנפה מחרוזת"
html_title:           "Arduino: מנפה מחרוזת"
simple_title:         "מנפה מחרוזת"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיזור על מחרוזות הוא תהליך שבו משתמשים במקום מחרוזת קבועה, התוכנית משתמשת במשתנה או ערך אחר לסגנון המחרוזת. תהליך זה מאפשר למתכנתים ליצור מחרוזות דינמיות ואינטראקטיביות, המתאימות על הרצת הקוד בזמן אמת.

## כיצד לעשות זאת?
נעזור בדוגמאות להמחשה של הבנת התהליך.

```Arduino
// דוגמא 1: חיזור על שם משתנה לתוך מחרוזת קבועה
int age = 25;
Serial.println("אני בן " + String(age) + " שנים");

// תוצאה: אני בן 25 שנים
```

```Arduino
// דוגמא 2: חיזור ערך בתוך מחרוזת
float temp = 36.5;
Serial.println("טמפרטורת החדר היא: " + String(temp) + " מעלות צלזיוס");

// תוצאה: טמפרטורת החדר היא: 36.5 מעלות צלזיוס
```

## חפירה מעמיקה
חיזור הוא תהליך נפוץ בעולם התכנות והוא מאפשר ליצור קוד יותר דינמי וקריא. תחילת המושג הוא קשור לשפת התכנות Perl מסוף שנות ה-80, אך תהליך זה התפתח והתפצל לתחומי תכנות שונים כמו Java ו-Python. יישומי תהליך נפוצים נוספים הם: תרגום, גרפיקה ומערכות ניהול תוכן.

## ראו גם
למידע נוסף על חיזור מחרוזות ב-Arduino, ניתן לקרוא את המדריך הרשמי של Arduino ולראות את מדריכי התכנות השונים ברשת העולמית.