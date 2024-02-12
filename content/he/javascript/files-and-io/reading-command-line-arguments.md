---
title:                "קריאת פרמטרים משורת הפקודה"
aliases: - /he/javascript/reading-command-line-arguments.md
date:                  2024-01-20T17:56:56.401057-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/reading-command-line-arguments.md"
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
