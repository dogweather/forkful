---
date: 2024-01-20 15:34:39.343520-07:00
description: "\u05E4\u05E8\u05E1\u05D5\u05E8 HTML \u05D4\u05D5\u05D0 \u05D4\u05EA\u05D4\
  \u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05E7\u05D5\u05D3 HTML \u05DE\u05D5\u05DE\
  \u05E8 \u05DC\u05DE\u05D1\u05E0\u05D4 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\
  \u05E0\u05D9\u05EA\u05DF \u05DC\u05D8\u05E4\u05DC \u05D1\u05D5 \u05D1\u05EA\u05D5\
  \u05DB\u05E0\u05D4. \u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA \u05DB\u05DE\u05D5\
  \ \u05DE\u05E0\u05D5\u05E2\u05D9 \u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05DB\u05DC\
  \u05D9 \u05E4\u05D9\u05EA\u05D5\u05D7 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8\
  \ \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05E4\u05E8\u05E1\u05D5\u05E8 \u05DB\u05D3\
  \u05D9 \u05DC\u05E0\u05EA\u05D7 \u05D3\u05E4\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8\u2026"
lastmod: '2024-03-11T00:14:12.345378-06:00'
model: unknown
summary: "\u05E4\u05E8\u05E1\u05D5\u05E8 HTML \u05D4\u05D5\u05D0 \u05D4\u05EA\u05D4\
  \u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05E7\u05D5\u05D3 HTML \u05DE\u05D5\u05DE\
  \u05E8 \u05DC\u05DE\u05D1\u05E0\u05D4 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\
  \u05E0\u05D9\u05EA\u05DF \u05DC\u05D8\u05E4\u05DC \u05D1\u05D5 \u05D1\u05EA\u05D5\
  \u05DB\u05E0\u05D4. \u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA \u05DB\u05DE\u05D5\
  \ \u05DE\u05E0\u05D5\u05E2\u05D9 \u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05DB\u05DC\
  \u05D9 \u05E4\u05D9\u05EA\u05D5\u05D7 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8\
  \ \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05E4\u05E8\u05E1\u05D5\u05E8 \u05DB\u05D3\
  \u05D9 \u05DC\u05E0\u05EA\u05D7 \u05D3\u05E4\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8\u2026"
title: "\u05E0\u05D9\u05EA\u05D5\u05D7 HTML"
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
