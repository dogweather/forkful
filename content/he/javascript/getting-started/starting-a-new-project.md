---
date: 2024-01-20 18:04:45.253477-07:00
description: "\u05DC\u05D4\u05EA\u05D7\u05D9\u05DC \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8\
  \ \u05D7\u05D3\u05E9 \u05D1-JavaScript \u05D6\u05D4 \u05DB\u05DE\u05D5 \u05DC\u05E4\
  \u05EA\u05D5\u05D7 \u05D3\u05E3 \u05D7\u05D3\u05E9; \u05D4\u05DE\u05E7\u05D5\u05DD\
  \ \u05DC\u05D4\u05D1\u05D9\u05D0 \u05DC\u05DE\u05E6\u05D9\u05D0\u05D5\u05EA \u05D0\
  \u05EA \u05D4\u05E8\u05E2\u05D9\u05D5\u05DF \u05E9\u05DC\u05DA. \u05EA\u05DB\u05E0\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4\
  \ \u05DB\u05D9 \u05D6\u05D4 \u05D4\u05D3\u05E8\u05DA \u05DC\u05D9\u05E6\u05D5\u05E8\
  \ \u05DE\u05E9\u05D4\u05D5 \u05D7\u05D3\u05E9, \u05DC\u05DC\u05DE\u05D5\u05D3, \u05D5\
  \u05DC\u05E4\u05EA\u05D5\u05E8 \u05D1\u05E2\u05D9\u05D5\u05EA."
lastmod: '2024-03-13T22:44:39.975742-06:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D4\u05EA\u05D7\u05D9\u05DC \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8\
  \ \u05D7\u05D3\u05E9 \u05D1-JavaScript \u05D6\u05D4 \u05DB\u05DE\u05D5 \u05DC\u05E4\
  \u05EA\u05D5\u05D7 \u05D3\u05E3 \u05D7\u05D3\u05E9; \u05D4\u05DE\u05E7\u05D5\u05DD\
  \ \u05DC\u05D4\u05D1\u05D9\u05D0 \u05DC\u05DE\u05E6\u05D9\u05D0\u05D5\u05EA \u05D0\
  \u05EA \u05D4\u05E8\u05E2\u05D9\u05D5\u05DF \u05E9\u05DC\u05DA."
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
weight: 1
---

## מה ולמה?
להתחיל פרויקט חדש ב-JavaScript זה כמו לפתוח דף חדש; המקום להביא למציאות את הרעיון שלך. תכנתנים עושים את זה כי זה הדרך ליצור משהו חדש, ללמוד, ולפתור בעיות.

## איך לעשות:
יצירת פרויקט חדש בקוד צריכה להיות פשוטה וישירה. להלן דוגמה להתחלת פרויקט פשוט עם Node.js וnpm (ניהול החבילות של Node).

```Javascript
// חדש ב-terminal, היכנס לתיקייה בה תרצה ליצור את הפרויקט
cd path_to_your_project_folder

// הקמת קובץ package.json חדש
npm init -y

// התקנת חבילה (לדוגמה express)
npm install express

// יצירת קובץ ראשי, למשל app.js
touch app.js

// כתבו את הקוד הבא בapp.js
const express = require('express');
const app = express();

app.get('/', (req, res) => {
  res.send('שלום עולם!');
});

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
  console.log(`השרת רץ על פורט ${PORT}`);
});
```

זהו. פרויקט קטן ב-JavaScript עם שרת אקספרס פעיל.

## זינוק עמוק:
לשנים, JavaScript היה שפת תיכנות רק לדפדפני רשת. עם הגעת Node.js ב-2009, המצב השתנה. כעת אפשר לכתוב קוד צד-שרת ב-JavaScript, מה שהפך פתיחת פרויקטים חדשים לאטרקטיבי יותר עבור מפתחים שלמים.

בנוסף, ישנם כלי ופלטפורמות רבות כמו Angular, React ו-Vue שעוזרים לבנות ממשקים ריך משתמש (UIs). יש לנו גם מערכות ניהול חבילות כמו npm ו-Yarn המקלות על ניהול תלות והתקנות.

כאשר מתחילים פרויקט חדש, תכנתנים צריכים לחשוב על דפוסי תכנון, ניהול מידע, בדיקות ויותר. זה לא רק לכתוב קוד, אלא לפתח מערכת בתור שלם.

## ראה גם:
הנה כמה משאבים שיכולים לעזור לכם להתחיל:

- [Node.js דוקומנטציה](https://nodejs.org/en/docs/)
- טוטוריאלים ל-Express: [Express.js דוקומנטציה](https://expressjs.com/)
- מבוא לניהול חבילות: [npm דוקומנטציה](https://docs.npmjs.com/)
- [MDN Web Docs](https://developer.mozilla.org/he/docs/Web/JavaScript) למידע על JavaScript בדפדפן
