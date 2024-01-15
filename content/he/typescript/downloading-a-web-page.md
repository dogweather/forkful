---
title:                "הורדת עמוד אינטרנט"
html_title:           "TypeScript: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מדוע

ישנם מספר סיבות שמובילות אנשים להוריד את דף האינטרנט שלהם, כגון לגישה לתוכן מקוון במצב לא מחובר או לשמור את התוכן לשימוש חוזר בעתיד.

## כיצד לעשות זאת

טיפוסקריפט מספק כלים להורדת דפי אינטרנט על ידי שימוש בפעולות כמו `fetch` או `XMLHttpRequest`. לדוגמה, הנה קוד שיחזיר את תוכן הדף של ה-Google בפורמט JSON:

```TypeScript
fetch('https://www.google.com')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.log(error));
```

הפלט של קוד זה יכול להיות משהו דומה לזה:

```
{
  "html": "<html>...</html>",
  "css": "body { ... }",
  "js": "function() { ... }"
}
```

## מעמקים נמוכים

למען הסר ספק, ישנן מספר מחלקות שניתן להשתמש בהם כדי להוריד דפי אינטרנט בטון יותר ותוך התייחסות לפרטים כמו כותרות, קובצי תמונות ועוד. כמו כן, טיפוסקריפט מאפשר התאמה גבוהה יותר של הבקשות ומתן יכולות נוספות כגון ניהול שגיאות וזיהוי פרטים רלוונטיים יותר בכתבי שגיאה.

## ראה גם

- [מדריך למידע נוסף על כלי ה-XHR של טיפוסקריפט](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [החומר המפורט על סיסמת כלי ה-JavaScript של טיפוסקריפט](https://www.typescriptlang.org/docs/handbook/security.html#middleware)
- [מאמר בנושא פעולות של ה-GitHub של טיפוסקריפט](https://github.com/Microsoft/TypeScript/wiki/TypeScpriptInternal1)