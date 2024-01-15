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

## למה

למה הייתם מעוניינים לשלוח בקשת HTTP? בכדי ליצור תקשורת בין שני מחשבים ברשת, לדוגמה כאשר אתם רוצים לבקש מידע מאתר אינטרנט או לשלוח נתונים לשרת.

## איך לעשות זאת

כדי לשלוח בקשת HTTP בשפת Clojure ניתן להשתמש בספריית `clj-http` שמאפשרת לכם ליצור בקשות GET, POST, PUT ו-DELETE. נציין לדוגמה איך לשלוח בקשת GET לאתר אינטרנט:

```Clojure
(ns http-example
  (:require [clj-http.client :as http]))

(defn http-get []
  (let [response (http/get "https://example.com"
                 :query-params {:name "John" :age "30"})]
    (println (:status response)) ; תחזיר 200 אם הבקשה הצליחה
    (println (:body response))))

(http-get)
```

תוצאת התכנית תהיה:

```Clojure
200
<html>
 ...
</html>
```

## השקעה עמוקה

בקשת HTTP הפכה לחלק לא נפרד מהאינטרנט ומתקשורת ברחבי העולם. היכולת לשלוח בקשות HTTP בשפת Clojure מאפשרת למתכנתים ליצור תכניות שמשתמשות בנתונים חיצוניים, מתאם לממשקים שונים וליצור יישומים מרתקים.

## ראו גם

- [מדריך Clojure המציג כיצד להשתמש בספריית clj-http](https://yogthos.net/posts/2015-10-22-HTTP-Request-with-Clojure.html)
- [ספריית clj-http המסייעת בשליחת בקשות HTTP בשפת Clojure](https://github.com/dakrone/clj-http)