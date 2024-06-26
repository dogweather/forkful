---
date: 2024-01-26 04:12:00.757314-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D7\
  \u05D9\u05DC \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD \u05DE\u05E0\u05E4\u05D4\
  \ \u05D1TypeScript, \u05DB\u05DC \u05DE\u05D4 \u05E9\u05D0\u05EA\u05D4 \u05E6\u05E8\
  \u05D9\u05DA \u05D6\u05D4 IDE \u05E0\u05EA\u05DE\u05DA (\u05DB\u05DE\u05D5 Visual\
  \ Studio Code) \u05D5\u05E7\u05D5\u05D1\u05E5 \u05EA\u05E6\u05D5\u05E8\u05D4 `launch.json`.\
  \ \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4 \u05DE\u05D4\u05D9\u05E8\u05D4\
  \ \u05DC\u05D9\u05D9\u05E9\u05D5\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.924955-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC \u05DC\u05E2\u05D1\
  \u05D5\u05D3 \u05E2\u05DD \u05DE\u05E0\u05E4\u05D4 \u05D1TypeScript, \u05DB\u05DC\
  \ \u05DE\u05D4 \u05E9\u05D0\u05EA\u05D4 \u05E6\u05E8\u05D9\u05DA \u05D6\u05D4 IDE\
  \ \u05E0\u05EA\u05DE\u05DA (\u05DB\u05DE\u05D5 Visual Studio Code) \u05D5\u05E7\u05D5\
  \u05D1\u05E5 \u05EA\u05E6\u05D5\u05E8\u05D4 `launch.json`."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D3\u05D9\u05D1\u05D0\u05D2\u05E8"
weight: 35
---

## איך ל:
כדי להתחיל לעבוד עם מנפה בTypeScript, כל מה שאתה צריך זה IDE נתמך (כמו Visual Studio Code) וקובץ תצורה `launch.json`. הנה דוגמה מהירה ליישום Node.js:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`שלום, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

כדי לנפות את זה, צור קובץ `launch.json` תחת תיקיית ה`.vscode`:

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "הפעלת תוכנית",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

לאחר מכן, הגדר נקודת עצירה בפונקציה `greet` שלך על ידי לחיצה על צד שמאל של מספר השורה ב-IDE שלך. לחץ על F5 כדי להתחיל בניפוי, ותצפה כיצד האפליקציה שלך עוצרת בנקודת העצירה. כעת אתה יכול להרחיף מעל משתנים, לצפות בביטויים, ולצעוד דרך הקוד שלך בקלות.

## צלילה עמוקה
בימים שלפני שסביבות הפיתוח המשולבות (IDEs) הפכו חלקות, ניפוי היה לעיתים קרובות נעשה באמצעות הודעות הדפסה (ידוע גם כ`console.log` debugging). זה עבד, במידת מה, אך היה כמו לנסות למצוא מחט בערמת קש מחובש.

מנפים מודרניים הם כמו סכין שוויצרית לפתרון בעיות. עם התפתחות TypeScript ו-Node.js, ישנם מנפים שונים זמינים, החל ממפקח הפנימי של Node.js ועד לכלים פיתוחיים של הדפדפן לניפוי בצד הלקוח.

מפקח ה-Node.js פועל על ידי התחברות ליישום הרץ שלך; הוא מתקשר דרך פרוטוקול Chrome DevTools, הופך את הדפדפן Chrome שלך לקונסולת ניפוי עוצמתית. האינטגרציה הזו מאפשרת מושב ניפוי מפורט ואינטראקטיבי מבחינה חזותית, לעומת הנהלים מסורתיים של ניפוי בשורת הפקודה.

## ראה גם
לקריאה נוספת וכמה טיפים מקצועיים, בדוק את:

- [ניפוי TypeScript ב-Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [מדריך ניפוי Node.js](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [תיעוד Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
