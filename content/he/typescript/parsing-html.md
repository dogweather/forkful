---
title:                "ניתוח HTML"
date:                  2024-01-20T15:34:39.343520-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פרסור HTML הוא התהליך שבו קוד HTML מומר למבנה נתונים שניתן לטפל בו בתוכנה. תוכניות כמו מנועי חיפוש וכלי פיתוח אינטרנט מבצעים פרסור כדי לנתח דפי אינטרנט ולאסוף מידע מהם.

## איך לעשות:
בדוגמה זו נשתמש במודול 'node-html-parser' לצורך פרסור HTML ב-TypeScript.

```TypeScript
import { parse } from 'node-html-parser';

// קטע HTML פשוט לדוגמה
const html = `
  <ul id="fruits">
    <li class="apple">Apple</li>
    <li class="banana">Banana</li>
  </ul>
`;

// פרסור ה-HTML והמרתו ל-DOM וירטואלי
const root = parse(html);

// הדפסת תוצאות פרקטיות
console.log(root.querySelector('.apple').innerText); // Apple
console.log(root.querySelector('.banana').textContent); // Banana
```

קוד ה-TypeScript הזה מדפיס `Apple` ואחריו `Banana` - הטקסט מתוך האלמנטים ב-HTML.

## עיון מעמיק
בהיסטוריה, פרסור HTML התבצע בעיקר על ידי דפדפנים כדי להציג דפי אינטרנט. בשנים האחרונות, גידול במספר מפתחים וכלים שמתעסקים ב-web scraping (איסוף נתונים אוטומטי מאתרים) ו-SEO (אופטימיזציה למנועי חיפוש) הביא לכך שיש צורך גובר בפרסור בצד הלקוח והשרת. ישנם שיטות חלופיות כמו ביטויים רגולריים אך הם לא מומלצות כיוון שהם פגיעים לשגיאות ופחות קריאים. פרסור HTML נכון דורש מתודולוגיה שחוזרת על הדרך בה הדפדפן עובד – כלומר, יצירת עץ DOM שמשקף את מבנה ה-HTML.

## ראה גם
- מדריך node-html-parser: https://github.com/taoqf/node-html-parser
- מידע נוסף על DOM: https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model
- מדריך ל-web scraping ב-TypeScript: https://dev.to/ryands17/web-scraping-in-typescript-5g7n
