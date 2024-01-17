---
title:                "הורדת דף אינטרנט"
html_title:           "Javascript: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# מה ולמה?
להוריד עמוד ווב הוא פעולה שמאפשרת למשתמשים לשמוע קבצים מן האינטרנט בצורה נוחה יותר. במהלך פיתוח אתרים ווב, פעולת ההורדה משמשת כדי להציג לקוחות את המידע הרצוי בצורה מהירה ויעילה. מתכנתים משתמשים בתהליך ההורדה כדי ליצור אפליקציות אינטראקטיביות ואתרי ווב מתקדמים.

## איך לעשות:
היישום ב-Javascript להורדת עמודי ווב הוא פשוט למדי. הנה כמה דוגמאות:
```Javascript
fetch('https://www.example.com')
  .then(response => response.text())
  .then(data => console.log(data));
```
הקוד הזה יבקש מהשרת להחזיר את התוכן של עמוד ה- "example.com" וידפיס אותו בטוחלג.

En deeper נייד:
לפני כמה שנים רחוקות, לעתים קרובות עמודי ווב נוצרו באמצעות טכנולוגיות עתיקות כמו ה- "XMLHttpRequest". עם זאת, בשנים האחרונות, מתכנתים מעדיפים להשתמש ב- "fetch" כי הוא מציע ביצוע יותר טוב ושימוש נוח יותר.

## עיון עמוק:
מעבר להורדת עמודי ווב, ישנן אפשרויות אלטרנטיביות נוספות כמו פרוטוקול HTTP ישן, בשימוש AJAX וישנן כמה ספריות כמו Axios ו- request שגם מציעות אפשרויות להורדת תמונות וקבצי וידאו.

## ראה גם:
למד נוסף על להורדת עמודי ווב בידיוק על ידי כתיבת קודי שאלות נפוצות מותאמים ומצריכי פעולה נוספים. כאן תוכל למצוא קוד בסיסי להורדת עמודי ווב ב-Javascript: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch