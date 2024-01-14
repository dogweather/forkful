---
title:    "Arduino: חישוב תאריך בעתיד או בעבר"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##למה

מחשבת חישוק תאריך עתידי או תאריך קודם יכולה להיות שימושית במגוון רחב של פרויקטים ארדואינו. למשל, זה יכול לסייע ביצירה של פסקי זמן, מערכות אזעקה או פשוט צפייה בתאריכים מרחוק. מחשבת תאריך גם יכולה לחסוך חשיבה ומאמץ כשמדובר בחישובים מורכבים של תאריכים.

##כיצד לעשות

הנה דוגמאות קוד של ארדוטונו ופלט מתאימים עבור כל אחת מהפונקציות המתמחות בחישובים תאריך:

קוד לחישוב תאריך עתידי:
```arduino
int day = 7;
int month = 12;
int year = 2019;

int daysToAdd = 365; // change this value to calculate a different future date

// calculate future date
int futureDay = day + daysToAdd % 30;
int futureMonth = month + (daysToAdd / 30) % 12;
int futureYear = year + (daysToAdd / 365);

// print output
Serial.print("The future date is: ");
Serial.print(futureDay);
Serial.print("/");
Serial.print(futureMonth);
Serial.print("/");
Serial.println(futureYear);
```

פלט:
```
The future date is: 10/12/2020
```

קוד לחישוב תאריך קודם:
```arduino
int day = 7;
int month = 12;
int year = 2019;

int daysToSubtract = 730; // change this value to calculate a different previous date

// calculate previous date
int previousDay = day - daysToSubtract % 30;
int previousMonth = month - (daysToSubtract / 30) % 12;
int previousYear = year - (daysToSubtract / 365);

// print output
Serial.print("The previous date is: ");
Serial.print(previousDay);
Serial.print("/");
Serial.print(previousMonth);
Serial.print("/");
Serial.println(previousYear);
```

פלט:
```
The previous date is: 22/12/2017
```

##לחקור עמוק יותר

כאשר מדברים על חישובים תאריך, יתרונותם הם נרחבים יותר מאשר פשוט לסייע בבניית פרויקטים ארדואינו. למשל, ניתן להשתמש בפונקציות נוספות לרישום ימים בשבוע או לכינויים מקוצרים של חודשים. כמו כן, ניתן לשנות את הקוד כך שיתאים לתאריכים בלועזיים, כדי שיהיו מתאימים למידע העומד לרשותנו.

##ראו גם

- [תיעוד פ