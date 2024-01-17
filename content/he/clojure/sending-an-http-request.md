---
title:                "שליחת בקשת http"
html_title:           "Clojure: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
שליחת בקשת HTTP היא פעולה נפוצה מאוד בתחום התכנות. היא מתאימה לשליחת מידע מהשרת ללקוח, כגון קבצים, תמונות או מידע דינמי. תכנתנים משתמשים בבקשות HTTP כדי ליצור יישומים רבי שימוש לצד הלקוח.

## איך לעשות?
בקשת HTTP נשלחת באמצעות פונקציית "clj-http.client/post". למשל:

```Clojure
(require '[clj-http.client :as client])
(client/post "http://example.com" {:form-params {:username "user" :password "pass"}})
```

תוצאת הפעולה תחזיר מידע מהשרת, כגון קבצים או מידע דינמי של האתר המבוקש.

## עומק ממעמקים
בקשות HTTP הן חלק בלתי נפרד מהעולם התעשייתי והתכנות. פונקציות נוספות כמו "GET" ו-"PUT" מאפשרות לקבל ולעדכן מידע מהשרת. פתאום כל העולם נמצא בכפתורים של המחשב שלנו.

## ראה גם
למידת Clojure: https://clojure.org/
מדריך לשליחת בקשות HTTP ב-Clojure: https://github.com/dakrone/cheshire/wiki/Sending-HTTP-Requests-with-Clojure