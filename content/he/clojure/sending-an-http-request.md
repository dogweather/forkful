---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא פעולה שבה מבקשים משרת מידע מסוים. מתכנתים משתמשים בו לדבר עם שרתים שונים, לשלוח נתונים ולקבל מידע חזרה.

## איך ל:
```clojure
(defn send-http-request
  [url]
  (let [client (http/create-client {})]
    (-> client
      (http/get url)
      (http/throw-unless 200))))

(println (send-http-request "http://example.com"))
```
ערך הפלט של הקוד הזה הוא מידע שהשרת מחזיר, כגון תוכן הדף של "http://example.com".

## עומק נוסף
משלוח בקשת HTTP היה חלק חשוב בהיסטוריה של האינטרנט מאז שהוצא לאור הגרסה הראשונה של הפרוטוקול HTTP ב-1991. בחרנו בדוגמא שמשתמשת ב-http-kit, אך ישנם אלטרנטיבות רבות אחרות, כמו clj-http או אפשרות לשליחת בקשות HTTP באמצעות Java או כלים נוספים. הקוד מפרט את הURL לו אנו מבקשים להתחבר, יוצר את הבקשה ושולח את הבקשה לשרת.

## ראה גם
- [HTTP Kit](http://www.http-kit.org/)
- [clj-http](https://github.com/dakrone/clj-http)