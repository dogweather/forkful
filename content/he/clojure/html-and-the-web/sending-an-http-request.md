---
date: 2024-01-20 17:59:47.431544-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D4\u05D9\
  \u05D0 \u05D3\u05E8\u05DA \u05DC\u05EA\u05E7\u05E9\u05E8 \u05E2\u05DD \u05E9\u05E8\
  \u05EA \u05D3\u05E8\u05DA \u05D4\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\
  \u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E9\u05D5\u05DC\u05D7\u05D9\u05DD\
  \ \u05D0\u05EA \u05D0\u05DC\u05D5 \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D7\u05D6\u05E8\
  \ \u05D0\u05D5 \u05DC\u05E9\u05DC\u05D5\u05D7 \u05DE\u05D9\u05D3\u05E2 \u05DC\u05D0\
  \u05EA\u05E8\u05D9\u05DD \u05D0\u05D5 \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9\u05DD\
  \ \u05DE\u05E8\u05D5\u05D7\u05E7\u05D9\u05DD."
lastmod: 2024-02-19 22:04:57.970242
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D4\u05D9\
  \u05D0 \u05D3\u05E8\u05DA \u05DC\u05EA\u05E7\u05E9\u05E8 \u05E2\u05DD \u05E9\u05E8\
  \u05EA \u05D3\u05E8\u05DA \u05D4\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05EA\
  \u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E9\u05D5\u05DC\u05D7\u05D9\u05DD\
  \ \u05D0\u05EA \u05D0\u05DC\u05D5 \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D7\u05D6\u05E8\
  \ \u05D0\u05D5 \u05DC\u05E9\u05DC\u05D5\u05D7 \u05DE\u05D9\u05D3\u05E2 \u05DC\u05D0\
  \u05EA\u05E8\u05D9\u05DD \u05D0\u05D5 \u05E9\u05D9\u05E8\u05D5\u05EA\u05D9\u05DD\
  \ \u05DE\u05E8\u05D5\u05D7\u05E7\u05D9\u05DD."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
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
