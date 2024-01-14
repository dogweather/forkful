---
title:                "TypeScript: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

כתיבת קוד היא תהליך מרתק ומאתגר בו אנו יוצרים תוכניות ואפליקציות מתקדמות ושימושיות. אחד המשימות הנפוצות בכתיבת קוד הינו שליחת בקשות HTTP. בכתבה הזו נלמד כיצד לשלוח בקשת HTTP באמצעות TypeScript ונלך על שטח דילוג עמוק בנושא.

## למה

שליחת בקשות HTTP הינה חלק חשוב מאוד בכתיבת אפליקציות תקשורת עם שרת וקבלת נתונים. זהו אמצעי חשוב להתקשרות עם שירותים חיצוניים ולשלוח נתונים למסדי נתונים. ללא שימוש בבקשות HTTP, יהיה קשה לבנות אפליקציות נגישות ומתחברות לרשת האינטרנט.

## כיצד לשלוח בקשת HTTP

דוגמה פשוטה לשליחת בקשת HTTP באמצעות TypeScript תראה כך:

```TypeScript
import axios from 'axios';

const url = 'https://jsonplaceholder.typicode.com/users';

axios.get(url)
    .then(response => console.log(response))
    .catch(error => console.log(error));
```
 פלט היוצא מתוך הקוד יכיל את כל המידע שנמצא בכתובת URL ויהיה זמין לשימוש באפליקציה שלנו.

אם נרצה לשלוח בקשת POST, ניתן לעשות זאת כך:

```TypeScript
import axios from 'axios';

const url = 'https://jsonplaceholder.typicode.com/users';

const user = {
    name: 'John',
    email: 'john@example.com'
};

axios.post(url, user)
    .then(response => console.log(response))
    .catch(error => console.log(error));
```

ניתן לראות שבקשת הPOST משתמשת במערך המשתמש שבנו ושולח אותו לכתובת URL כדי לשמור אותו.

## דילוג עמוק

כעת שלמדנו כיצד לשלוח בקשות HTTP באמצעות TypeScript, אנו יכולים ללכת על שטח דילוג עמוק יותר וליצור כתיבה יעילה ומתוחכמת יותר של בקשות הביצוע.

בקשות הביצוע הן כאלו שמכילות טקסט, תמונות, וידאו או כל סוג אח