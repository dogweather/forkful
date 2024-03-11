---
date: 2024-01-26 04:19:16.897251-07:00
description: "\u05DC\u05D5\u05DC\u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4-\u05D4\
  \u05E2\u05E8\u05DB\u05D4-\u05D4\u05D3\u05E4\u05E1\u05D4 (REPL) \u05D4\u05D9\u05D0\
  \ \u05E1\u05D1\u05D9\u05D1\u05EA \u05EA\u05DB\u05E0\u05D5\u05EA \u05E9\u05DE\u05E7\
  \u05D1\u05DC\u05EA \u05E7\u05DC\u05D8\u05D9\u05DD \u05D1\u05D5\u05D3\u05D3\u05D9\
  \u05DD \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\u05E9, \u05DE\u05D1\u05E6\u05E2\u05EA\
  \ \u05D0\u05D5\u05EA\u05DD \u05D5\u05DE\u05D7\u05D6\u05D9\u05E8\u05D4 \u05D0\u05EA\
  \ \u05D4\u05EA\u05D5\u05E6\u05D0\u05D4 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\
  \ \u05D1-REPL \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E1\u05D5\u05EA \u05E7\u05D8\u05E2\
  \u05D9 \u05E7\u05D5\u05D3\u2026"
lastmod: '2024-03-11T00:14:12.351917-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05D5\u05DC\u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4-\u05D4\u05E2\
  \u05E8\u05DB\u05D4-\u05D4\u05D3\u05E4\u05E1\u05D4 (REPL) \u05D4\u05D9\u05D0 \u05E1\
  \u05D1\u05D9\u05D1\u05EA \u05EA\u05DB\u05E0\u05D5\u05EA \u05E9\u05DE\u05E7\u05D1\
  \u05DC\u05EA \u05E7\u05DC\u05D8\u05D9\u05DD \u05D1\u05D5\u05D3\u05D3\u05D9\u05DD\
  \ \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\u05E9, \u05DE\u05D1\u05E6\u05E2\u05EA \u05D0\
  \u05D5\u05EA\u05DD \u05D5\u05DE\u05D7\u05D6\u05D9\u05E8\u05D4 \u05D0\u05EA \u05D4\
  \u05EA\u05D5\u05E6\u05D0\u05D4 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\
  -REPL \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E1\u05D5\u05EA \u05E7\u05D8\u05E2\u05D9\
  \ \u05E7\u05D5\u05D3\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
---

{{< edit_this_page >}}

## מה ולמה?
לולאת קריאה-הערכה-הדפסה (REPL) היא סביבת תכנות שמקבלת קלטים בודדים מהמשתמש, מבצעת אותם ומחזירה את התוצאה למשתמש. מתכנתים משתמשים ב-REPL כדי לנסות קטעי קוד במהירות, לנפות תקלות וללמוד תכונות שפה חדשות בלי הצורך ביצירת אפליקציה מלאה.

## איך לכך:
TypeScript אינו מגיע עם REPL משלו. בואו נשתמש ב- `ts-node`, סביבת הפעלה של TypeScript עבור Node.js הכוללת REPL.

ראשית, התקינו אותו באופן גלובלי:
```bash
npm install -g ts-node
```

התחילו את ה-REPL על ידי הקלדת `ts-node` בשורת הפקודה שלכם:
```bash
ts-node
```

הנה קטע קוד זריז לניסוי:
```TypeScript
> let message: string = 'Hello, REPL!';
> console.log(message);
Hello, REPL!
> 
```
כדי לסיים את המושב, לחצו על `Ctrl+D`.

## צלילה עמוקה
בהיסטוריה, REPLs היו בולטות בשפות כמו Lisp, שמאפשרות הערכה דינמית של קוד. המושג מאז התפשט, והפך לבסיסי לקידוד אינטראקטיבי בשפות רבות.

עבור TypeScript, `ts-node` אינו האופציה היחידה שלכם. אלטרנטיבות כוללות שימוש ב- TypeScript Playground בדפדפן או בשימוש ב-REPLs אחרות המבוססות על Node.js שתומכות ב-TypeScript עם תוספים מתאימים.

מבחינת ביצוע, `ts-node` משתמש ב-API של מהדר TypeScript כדי לתרגם את הקוד באופן מיידי לפני הרצתו על ידי Node.js. זה נותן לכם משוב מיידי ומועיל במיוחד לניסוי עם התכונות האחרונות של TypeScript בלי טרחות הקמה.

דבר אחד לזכור – בעוד ש-REPL נהדר לבדיקות מהירות, זה לא מחליף כתיבת קוד מסורתי, ניתן לבדיקה ונתמך. זהו כלי ללמידה ולחקר, ולא תחליף לשיטות פיתוח נאותות.

## ראו גם
- [אתר הרשמי של TypeScript](https://www.typescriptlang.org/)
- [ts-node ב-GitHub](https://github.com/TypeStrong/ts-node)
- [תיעוד REPL של Node.js](https://nodejs.org/api/repl.html)
- [TypeScript Playground](https://www.typescriptlang.org/play)
