---
aliases:
- /he/google-apps-script/using-regular-expressions/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:50.740352-07:00
description: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D4\u05DD \u05EA\u05D1\u05E0\u05D9\u05D5\u05EA \u05D4\
  \u05DE\u05E9\u05DE\u05E9\u05D5\u05EA \u05DC\u05D4\u05EA\u05D0\u05DE\u05D4 \u05E9\
  \u05DC \u05E9\u05D9\u05DC\u05D5\u05D1\u05D9 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\u05DD \u05DC\u05D7\
  \u05D9\u05E4\u05D5\u05E9, \u05E2\u05E8\u05D9\u05DB\u05D4, \u05D0\u05D5 \u05E9\u05D9\
  \u05E0\u05D5\u05D9 \u05D8\u05E7\u05E1\u05D8 \u05D5\u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD, \u05DE\u05D4 \u05E9\u05D4\u05D5\u05E4\u05DA \u05D0\u05D5\u05EA\u05DD\u2026"
lastmod: 2024-02-18 23:08:52.377641
model: gpt-4-0125-preview
summary: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D4\u05DD \u05EA\u05D1\u05E0\u05D9\u05D5\u05EA \u05D4\
  \u05DE\u05E9\u05DE\u05E9\u05D5\u05EA \u05DC\u05D4\u05EA\u05D0\u05DE\u05D4 \u05E9\
  \u05DC \u05E9\u05D9\u05DC\u05D5\u05D1\u05D9 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\u05DD \u05DC\u05D7\
  \u05D9\u05E4\u05D5\u05E9, \u05E2\u05E8\u05D9\u05DB\u05D4, \u05D0\u05D5 \u05E9\u05D9\
  \u05E0\u05D5\u05D9 \u05D8\u05E7\u05E1\u05D8 \u05D5\u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD, \u05DE\u05D4 \u05E9\u05D4\u05D5\u05E4\u05DA \u05D0\u05D5\u05EA\u05DD\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D9\u05DC\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

ביטויים רגולריים (regex) הם תבניות המשמשות להתאמה של שילובי תווים במחרוזות. מתכנתים משתמשים בהם לחיפוש, עריכה, או שינוי טקסט ונתונים, מה שהופך אותם לחיוניים למשימות של התאמת תבניות וניתוח נתונים.

## איך לעשות:

השימוש בביטויים רגולריים ב-Google Apps Script הוא פשוט בזכות התחביר המבוסס על JavaScript. הנה איך תוכלו לשלב regex בסקריפטים שלכם למטלות נפוצות כמו חיפוש ואימות נתונים.

### חיפוש במחרוזות

נניח שאתם רוצים למצוא אם מחרוזת מכילה תבנית מסוימת, כמו כתובת דואר אלקטרוני. הנה דוגמא פשוטה:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("נמצא: " + found[0]);
  } else {
    Logger.log("לא נמצא דואר אלקטרוני.");
  }
}

// שימוש בדוגמה
findEmailInText("צרו קשר ב-info@example.com.");
```

### אימות נתונים

ביטויים רגולריים בולטים באימות נתונים. למטה פונקציה שמוודאת שמחרוזת קלט נתונה עומדת בפוליסת סיסמה פשוטה (לפחות אות גדולה אחת, אות קטנה אחת, ומינימום של 8 תווים).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// פלט לדוגמה
Logger.log(validatePassword("Str0ngPass")); // מוציא: true
Logger.log(validatePassword("weak"));       // מוציא: false
```

## צלילה עמוקה

ביטויים רגולריים ב-Google Apps Script יורשים מ-JavaScript, שהוסטנדרט לראשונה במפרט שפת ECMAScript ביוני 1997. למרות שהם עוצמתיים, לעיתים הם יכולים להוביל לקוד מבלבל וקשה לתחזוק, במיוחד כאשר משתמשים בהם יתר על המידה או למשימות מורכבות של התאמת תבניות שאולי יפתרו בצורה יעילה יותר דרך שיטות ניתוח אחרות.

לדוגמא, בעוד שניתן להשתמש ב-regex לניתוח HTML או XML במקרה הצורך, הדבר מומלץ פחות בשל המבנים המקוננים והמורכבים של מסמכים אלו. במקום זה, כלים שתוכננו במיוחד לניתוח מבנים כאלה, כמו פרשני DOM ל-HTML, הם יותר אמינים וקריאים.

בנוסף, מפתחי Google Apps Script צריכים להיות מודעים לבעיות ביצועים פוטנציאליות כאשר משתמשים בתבניות regex מורכבות במשימות טיפול בטקסט בסדר גודל גדול, כיוון שעיבוד regex יכול להיות עומס על המעבד. במקרים כאלה, חלוקה של המשימה לתתי-משימות פשוטות יותר או שימוש בפונקציות מובנות של טיפול במחרוזות יכולות להציע איזון טוב יותר של ביצועים וניתנות לתחזוקה.
