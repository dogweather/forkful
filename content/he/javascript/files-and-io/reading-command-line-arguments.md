---
date: 2024-01-20 17:56:56.401057-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D4\u05D9\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D4\
  \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8 \u05E9\u05DC\u05E0\u05D5 \u05DE\u05E7\u05D1\
  \u05DC \u05DE\u05D9\u05D3\u05E2 \u05DE\u05D1\u05D7\u05D5\u05E5 \u05D3\u05E8\u05DA\
  \ \u05D4\u05D8\u05E8\u05DE\u05D9\u05E0\u05DC. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05D2\u05D3\u05D9\u05E8 \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05D5\u05DC\u05E9\u05DC\u05D5\u05D8 \u05D1\u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\
  \u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05D1\u05E2\u05EA\u2026"
lastmod: '2024-02-25T18:49:38.242337-07:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D4\u05D9\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D4\
  \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8 \u05E9\u05DC\u05E0\u05D5 \u05DE\u05E7\u05D1\
  \u05DC \u05DE\u05D9\u05D3\u05E2 \u05DE\u05D1\u05D7\u05D5\u05E5 \u05D3\u05E8\u05DA\
  \ \u05D4\u05D8\u05E8\u05DE\u05D9\u05E0\u05DC. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05D2\u05D3\u05D9\u05E8 \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05D5\u05DC\u05E9\u05DC\u05D5\u05D8 \u05D1\u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\
  \u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05D1\u05E2\u05EA\u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה היא התהליך שבו הסקריפט שלנו מקבל מידע מבחוץ דרך הטרמינל. תכניתנים עושים זאת כדי להגדיר פרמטרים ולשלוט בהתנהגות התוכנית בעת הרצתה.

## איך לעשות:
בואו נדבר על `process.argv`, הערה שמאפשרת לך לקלוט ארגומנטים משורת הפקודה בNode.js.

```javascript
// script.js

// הדפס את כל הארגומנטים שהועברו לסקריפט (כולל נתיב לnode ולסקריפט)
console.log(process.argv);

// השתמש בארגומנטים שהועברו מתוך שורת הפקודה (ללא השניים הראשונים)
const args = process.argv.slice(2);
console.log(args);
```

הרץ את הסקריפט כך: `node script.js hello world`

תוצאה:
```
[
  '/usr/local/bin/node',
  '/path/to/your/script.js',
  'hello',
  'world'
]
['hello', 'world']
```

## נסיון עמוק:
השימוש ב־`process.argv` הוא די בסיסי ומיושן, אך עדיין מאוד פופולרי. בעבר, לפני שהיו ספריות עשירות, זו הייתה הדרך העיקרית להעביר מידע לסקריפטים. היום יש אופציות אחרות כמו ספריות `minimist`, `commander`, או `yargs` שכל אחת מהן מציעה פרסינג רב יותר עוצמתי ופשוט לשימוש. ביישומים מורכבים, ניתן להשתמש בסביבות הגדרת המשתמש (dotenv) וקבצי תצורה לניהול הגדרות.

## ראה גם:
- [Node.js process.argv documentation](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Minimist npm package](https://www.npmjs.com/package/minimist)
- [Commander npm package](https://www.npmjs.com/package/commander)
- [Yargs npm package](https://www.npmjs.com/package/yargs)
- [dotenv npm package](https://www.npmjs.com/package/dotenv)
