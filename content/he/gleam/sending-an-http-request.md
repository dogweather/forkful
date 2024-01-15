---
title:                "שליחת בקשת http"
html_title:           "Gleam: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

# למה:

כשפותחים אפליקציות או אתרים באינטרנט, לעיתים קרובות מופיעה הצורך לשלוח בקשות HTTP. הן משמשות כדי להתחבר לשרתים ולקבל מידע או לשלוח עדכונים לשרת. ללא כישורים בשליטה על בקשות ה-HTTP, יכול להיות קשה לפתח אפליקציות רבות ותכניות צד לקוח.

# איך לעשות זאת:

כדי לשלוח בקשות HTTP באמצעות Gleam יש להשתמש בפונקציית `httpc.send`. הנה דוגמא לשליחת בקשת GET לאתר של גוגל:

```
Gleam ->
  let
    url = "https://www.google.com"
    request = httpc.get(url)
  in
  request
```

הקוד מגיע בעזרת `get` לשכבת האפליקציה התחתונה ומחזיר את התשובה של השרת לאחר שורת ההתחברות נטענת. בדרך זו ניתן לשלוח גם בקשות POST ולהוסיף פרמטרים לגוף הבקשה. 

# לחקור עמוק יותר:

כאשר משתמשים ב-HTTP requests ב- Gleam, ניתן לשלוח כמה בקשות גם במקביל באמצעות פונקציות כמו `send_all` ו`pipe`. כמו כן, ניתן להגדיר כשירות HTTP משלנו באמצעות ספריית `gkoa`, שמאפשרת לנו לבנות מתאם לשירות ולנהל בקשות עם פרמטרים נוספים כגון תזמון והענקת הרשאות.

# ראה גם:

- דוגמאות נוספות של שליחת בקשות HTTP בעזרת Gleam ניתן למצוא במסמכי הסיוע של Gleam.
- בכתובת האינטרנט הבאה ניתן למצוא הסבר נרחב על שימוש ב-HTTP requests ב-Gleam: https://gleam.run/docs/http-client.