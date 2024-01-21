---
title:                "התחלת פרויקט חדש"
date:                  2024-01-20T18:04:45.253477-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

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