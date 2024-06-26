---
date: 2024-01-20 17:57:47.253799-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-TypeScript,\
  \ \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\
  `process.argv` \u05DB\u05D3\u05D9 \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D0\u05E8\u05D2\
  \u05D5\u05DE\u05E0\u05D8\u05D9\u05DD. \u05E0\u05EA\u05D7\u05D9\u05DC \u05E2\u05DD\
  \ \u05D3\u05D5\u05D2\u05DE\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4."
lastmod: '2024-03-13T22:44:38.943310-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-TypeScript, \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05E9\u05EA\u05DE\
  \u05E9\u05D9\u05DD \u05D1`process.argv` \u05DB\u05D3\u05D9 \u05DC\u05E7\u05E8\u05D5\
  \u05D0 \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\u05D9\u05DD."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
weight: 23
---

## איך לעשות:
ב-TypeScript, אנחנו משתמשים ב`process.argv` כדי לקרוא ארגומנטים. נתחיל עם דוגמה פשוטה:

```TypeScript
// index.ts

// הדפס את כל הארגומנטים
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});
```

רץ את הקוד עם `ts-node index.ts arg1 arg2 arg3` ותקבל:

```
0: path/to/node
1: path/to/index.ts
2: arg1
3: arg2
4: arg3
```

אם אתה רוצה רק את הארגומנטים של המשתמש (ולא את הנתיבים), התחל מהאינדקס השלישי:

```TypeScript
// index.ts

const userArgs = process.argv.slice(2);

console.log(userArgs); // ['arg1', 'arg2', 'arg3']
```

## צלילה עמוקה
קריאת ארגומנטים משורת הפקודה זה לא חידוש. ברוב שפות התכנות, זה מנגנון יסודי. ב-TypeScript, שהוא למעשה JavaScript מוכתב, אנו משתמשים בגישה הדומה של `process.argv` שיש לנו ב-Node.js.

חלופות לקריאת ארגומנטים קיימות. לדוגמה, חבילות כמו `yargs` או `commander` מספקות יותר גמישות ואפשרויות לפרסון של ארגומנטים.

פרט לכך, קריאה ישירה מ-`process.argv` יכולה להיות גולמית ולדרוש הרבה קוד ניתוח משלך. שים לב שכל ארגומנט נחשב כטקסט, אז אם אתה צריך להמיר לסוגים אחרים (כמו מספרים), תצטרך לעשות זאת בעצמך.

## ראה גם
- [Node.js process.argv documentation](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Yargs GitHub repository](https://github.com/yargs/yargs) - כדי להבין איך להשתמש ב`yargs` לניתוח ארגומנטים.
- [Commander GitHub repository](https://github.com/tj/commander.js) - עוד אפשרות פופולרית לניהול ארגומנטים בצורה מסודרת.
