---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:15.821437-07:00
description: "\u05D0\u05D9\u05EA\u05D5\u05E8 \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Google Apps Script, \u05E9\u05E4\u05EA\
  \ \u05EA\u05E1\u05E8\u05D9\u05D8 \u05E2\u05E0\u05DF \u05E9\u05DC JavaScript \u05E9\
  \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DA \u05DC\u05D0\u05D5\u05D8\u05DE\
  \u05D8 \u05D0\u05EA \u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05D1\u05DE\u05D5\u05E6\
  \u05E8\u05D9 Google, \u05DE\u05D3\u05D5\u05D1\u05E8 \u05E2\u05DC \u05E7\u05D1\u05D9\
  \u05E2\u05EA \u05DE\u05E1\u05E4\u05E8 \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\
  \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.549242-06:00'
model: gpt-4-0125-preview
summary: "\u05D0\u05D9\u05EA\u05D5\u05E8 \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Google Apps Script, \u05E9\u05E4\u05EA \u05EA\
  \u05E1\u05E8\u05D9\u05D8 \u05E2\u05E0\u05DF \u05E9\u05DC JavaScript \u05E9\u05DE\
  \u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DA \u05DC\u05D0\u05D5\u05D8\u05DE\u05D8\
  \ \u05D0\u05EA \u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05D1\u05DE\u05D5\u05E6\u05E8\
  \u05D9 Google, \u05DE\u05D3\u05D5\u05D1\u05E8 \u05E2\u05DC \u05E7\u05D1\u05D9\u05E2\
  \u05EA \u05DE\u05E1\u05E4\u05E8 \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\u05D4\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05DB\u05D9\u05DC\u05D4."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05D4\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 7
---

## איך לעשות:
ב-Google Apps Script, ניתן למצוא את אורך המחרוזת באמצעות התכונה `.length`, בדומה ל-JavaScript. תכונה זו מחזירה את מספר התווים שבתוך המחרוזת, כולל רווחים ותווים מיוחדים. להלן כמה דוגמאות:

```javascript
// הגדרת מחרוזת
var text = "Hello, World!";
// חיפוש אורך המחרוזת
var length = text.length;
// הדפסת האורך
Logger.log(length); // פלט: 13
```

בסיטואציות שבהן אתה עובד עם קלט משתמש מתוך טפסים או גיליונות של Google, איתור אורך המחרוזת עוזר באימות נתונים:

```javascript
// קלט מחרוזת דוגמה ממשתמש ב-Google Sheets
var userEntry = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet().getRange("A1").getValue();
// חישוב והדפסה של אורך הקלט
Logger.log(userEntry.length); // הפלט תלוי בתוכן של תא A1
```

בואו נוסיף דוגמה מעשית הכוללת תנאי. אם הקלט עולה על אורך מסוים, ייתכן ותרצו לזרוק שגיאה או אזהרה:

```javascript
var comment = "This is a sample comment that is too long for our database.";
if(comment.length > 50) {
  Logger.log("שגיאה: התגובה שלך לא צריכה לעבור את 50 התווים.");
} else {
  Logger.log("תודה על הגשתך.");
}
// פלט: שגיאה: התגובה שלך לא צריכה לעבור את 50 התווים.
```

## הפניה לעומק
בהקשר של Google Apps Script, שמבוססת על JavaScript, התכונה `.length` מגיעה מהתקן ECMAScript, שקובע את מפרטי JavaScript. התכונה `.length` היא חלק מ-JavaScript משלביו הראשוניים, מספקת דרך פשוטה להעריך את הגודל של מחרוזת.

פרט נוסף ששווה לציין הוא ש-Google Apps Script מתבצעת על שרתי Google, ולא בדפדפן. זה אומר שכאשר אתה עוסק במחרוזות ובאורכן, במיוחד בערכות נתונים גדולות שנאספות מ-Google Sheets או Docs, זמן הביצוע עלול להיות מושפע עקב עיכובי רשת ומגבלות זמן ריצה של התסריטים.

בעוד `.length` היא שיטה ישירה ונפוצה למציאת אורך מחרוזת, אסטרטגיות חלופיות עשויות לכלול regex או עיבור דרך מחרוזת לספירת תווים, במיוחד כאשר מתמודדים עם תווים בגודל מולטי-בייט או כאשר צריך לסנן סוגים מסוימים של תווים. עם זאת, לרוב המטרות המעשיות בתוך Google Apps Script, `.length` מספקת דרך אמינה ויעילה לקבוע את אורך המחרוזת.

תמיד זכור, במיוחד ב-Google Apps Script, לשקול את ההקשר שבו אתה מריץ את הקוד שלך. תפוקה ומגבלות ביצוע עשויות להנחות אותך לשפר את תהליכי התמודדות עם מחרוזות, כולל איך שאתה קובע את אורכן.
