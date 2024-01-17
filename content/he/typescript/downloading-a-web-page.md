---
title:                "הורדת דף אינטרנט"
html_title:           "TypeScript: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

"מה ולמה?"

להוריד דף אינטרנט הוא פעולה שמאפשרת לנו לקבל את קוד המקור של דף אינטרנט מהאתר המרוחק. פעולה זו מאפשרת לנו לעבד את המידע שנמצא בדף אינטרנט ולהציג אותו בצורה שאנחנו רוצים. תכניתנים עושים זאת בכדי ליצור אפליקציות או כלי אוטומציה שצריך להשתמש בנתונים מדף אינטרנט מסוים.

"איך לעשות?"

```TypeScript
import fetch from 'node-fetch';

fetch('https://www.example.com')
  .then((response) => response.text())
  .then((html) => console.log(html));
```

את הדף אינטרנט מורידים באמצעות פונקציית ה-fetch של ספריית node-fetch. הפונקציה מחזירה לנו תוצאת Promise ואז משתמשים בפונקציות קולבק כדי להתייחס למידע המושג בפעמים הבאות. בדוגמה שלנו, אנחנו מדפיסים את מידע ה-HTML שנמצא בדף האינטרנט.

"עולם מתמעמע"

להוריד דפי אינטרנט הינה פעולה שמתבצעת כבר שנים רבות והפונקציונליות שלה חלק מכלי הפיתוח החשובים ביותר. ישנן גם אלטרנטיבות להורדת דפים כגון פרוטוקול כמו https ופקודות Shell מסוג wget או cURL. מימוש הפונקציה כולל שימוש בפרוטוקול HTTP וקבלת התגובה מהשרת.

"ראו גם"

למידע נוסף על פונקציית ה-fetch ניתן לעיין בתיעוד הרשמי של TypeScript: https://www.typescriptlang.org/docs/handbook/enums.html

לדוגמה נוספת והסברים על איך להתמודד עם שגיאות נפוצות בהורדת דף אינטרנט, ניתן לבקר במאמר הבא: https://blog.bitsrc.io/everything-you-need-to-know-about-browser-page-downloads-169dff68ca67