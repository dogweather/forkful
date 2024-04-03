---
date: 2024-01-26 01:01:15.707217-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DC-Arduino \u05D0\
  \u05D9\u05DF \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05E8\u05D9\u05E9\u05D5\u05DD\
  \ \u05D8\u05D5\u05D1\u05E2\u05EA \u05D1\u05D3\u05D5\u05DE\u05D4 \u05DC\u05E1\u05D1\
  \u05D9\u05D1\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA, \u05D0\u05D1\u05DC \u05D0\
  \u05E4\u05E9\u05E8 \u05DC\u05DE\u05DE\u05E9 \u05E8\u05D9\u05E9\u05D5\u05DD \u05D1\
  \u05E1\u05D9\u05E1\u05D9 \u05DC\u05E7\u05D5\u05E0\u05E1\u05D5\u05DC\u05EA \u05D4\
  -Serial \u05D1\u05E4\u05D7\u05D5\u05EA \u05DE\u05D0\u05DE\u05E5. \u05D4\u05E0\u05D4\
  \ \u05D3\u05D5\u05D2\u05DE\u05D0 \u05D6\u05E8\u05D9\u05D6\u05D4 \u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC."
lastmod: '2024-03-13T22:44:39.777370-06:00'
model: gpt-4-1106-preview
summary: "\u05DC-Arduino \u05D0\u05D9\u05DF \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05E8\
  \u05D9\u05E9\u05D5\u05DD \u05D8\u05D5\u05D1\u05E2\u05EA \u05D1\u05D3\u05D5\u05DE\
  \u05D4 \u05DC\u05E1\u05D1\u05D9\u05D1\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA\
  , \u05D0\u05D1\u05DC \u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\u05DE\u05E9 \u05E8\u05D9\
  \u05E9\u05D5\u05DD \u05D1\u05E1\u05D9\u05E1\u05D9 \u05DC\u05E7\u05D5\u05E0\u05E1\
  \u05D5\u05DC\u05EA \u05D4-Serial \u05D1\u05E4\u05D7\u05D5\u05EA \u05DE\u05D0\u05DE\
  \u05E5."
title: "\u05DC\u05D5\u05D2\u05D9\u05DD"
weight: 17
---

## איך לעשות:
ל-Arduino אין ספריית רישום טובעת בדומה לסביבות אחרות, אבל אפשר לממש רישום בסיסי לקונסולת ה-Serial בפחות מאמץ. הנה דוגמא זריזה כדי להתחיל:

```arduino
void setup() {
  // התחלת תקשורת סריאלית עם קצב המז"ב הנתון
  Serial.begin(9600);

  // המתן לחיבור הפורט הסריאלי - נחוץ רק בכמה לוחות
  while (!Serial) {
    ; // המתן לחיבור הפורט הסריאלי. נחוץ ל-USB מקורי
  }

  // רישום הודעת מידע המציינת שתהליך ההכנה הסתיים
  Serial.println("הכנה הושלמה!");
}

void loop() {
  // רישום פשוט שמדפיס את זמן הפעולה (uptime) כל שנייה
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("זמן הפעולה (ms): ");
    Serial.println(currentMillis);

    // כאן תוכל להוסיף גם יומני שגיאות, אזהרות, או מידע נוסף.
  }
  
  // שאר הלוגיקה של התוכנית שלך כאן...
}
```

פלט דוגמא של Serial:
```
הכנה הושלמה!
זמן הפעולה (ms): 1000
זמן הפעולה (ms): 2000
זמן הפעולה (ms): 3000
...
```

## עיון נוסף:
בהיסטוריה, רישום לוגים במיקרו-בקרים לא היה פשוט כפי שהיה במערכת הפעלה מורכבת. משאבים מוגבלים אילצו להיות חזקים על כל בית ופיתוח נדרש להיזהר שלא לסתום את המערכת. עם בואם של לוחות יותר יכולים והפלטפורמה של ארדואינו שפישטה את התהליך, רישום נעשה נגיש יותר.

בעוד הקוד לעיל מציג רישום דרך הממשק Serial, ישנם שיטות אחרות כמו כתיבה לכרטיס SD, שליחת נתונים ברשת לשרת מרוחק, או אפילו פלט למסך LCD קטן.

הטמעת מערכת רישומים מעלה שיקולים כמו סיבוב קבצים, חומרת הרמה (מידע, ניפוי, אזהרה, שגיאה), והשפעה על הביצועים. בארדואינו, ייתכן שתצטרך להיות ער למגבלות זיכרון כאשר תרשום מבני נתונים מורכבים. לשיקול דעת של שליחת יומנים מרוחקים, אבטחת היומנים שנשלחים היא חשש נוסף.

פתרונות מתקדמים יותר כמו Syslog, תקן רישום שהתקבל באופן נרחב, קיימים מחוץ לעולם ארדואינו, אך אפשר לשלב ספריות צד שלישי המציעות פונקציונליות דומה עם רמות שונות של מורכבות ודרישות משאבים.

## ראה גם:
- [הפניה ל-'Serial' של ארדואינו](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [רישום יומן בעזרת כרטיס SD עם ארדואינו](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [מגן לרישום נתונים של SparkFun](https://www.sparkfun.com/products/13712)
- [TinyWeb: דוגמא מעשית של רישום מרוחק עם ארדואינו](https://www.arduino.cc/en/Tutorial/WebClientRepeating)
