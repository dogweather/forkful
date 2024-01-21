---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T17:59:47.431544-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא דרך לתקשר עם שרת דרך האינטרנט. תוכניתנים שולחים את אלו כדי לאחזר או לשלוח מידע לאתרים או שירותים מרוחקים.

## איך לעשות:
שליחת בקשת HTTP בClojure היא פשוטה. עם ספריית clj-http, הקוד קצר ולעניין.

```Clojure
(require '[clj-http.client :as client])

; שליחת בקשת GET
(def response (client/get "http://example.com"))
(println (:status response))
(println (:headers response))
(println (:body response))
```
תוצאת הדוגמא:
```
200
{"Content-Type" "text/html; charset=UTF-8", ...}
"<html> ... </html>"
```
לשליחת בקשת POST, פשוט הוסף מפרמטים.
```Clojure
(def post-response (client/post "http://example.com/post"
  {:form-params {:key1 "value1" :key2 "value2"}}))
(println (:status post-response))
```
תוצאת הדוגמא:
```
200
```
## צלילה עמוקה
שליחת בקשות HTTP היא חלק חשוב בתקשורת ברשת מאז תחילת האינטרנט. בעבר נעשה שימוש בספריות כמו java.net.* שב-Java, אך ב-Clojure מעדיפים ספריות פשוטות ויעילות כמו `clj-http`. `clj-http` מסתמכת על Apache HttpClient לפרטי המימוש ומוסיפה ממשק פשוט ונוח ל-Clojure.

יתרונות `clj-http` כוללים התממשקות נוחה עם מבני נתונים של Clojure, ניהול אוטומטי של קוקיז וחיבורים, ודינמיות בשליחת בקשות - ניתן לשלב קלוז'רים להתאמה אישית של התהליך.

יש גם אלטרנטיבות אחרות ל-shlj-http כמו `http-kit` ו`aleph` שמציעות מודלים שונים של תקשורת אסינכרונית ותומכות ב-HTTP/2.

## ראה גם:
- התיעוד של `clj-http`: https://github.com/dakrone/clj-http
- מדריך ל`http-kit`: http://www.http-kit.org
- דף הגיטהאב של `aleph`: https://github.com/ztellman/aleph
- פרטים על ספריית Apache HttpClient ש`clj-http` משתמשת בה: https://hc.apache.org/httpcomponents-client-ga/