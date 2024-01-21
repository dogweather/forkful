---
title:                "הורדת דף אינטרנט"
date:                  2024-01-20T17:43:36.772992-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
להוריד דף אינטרנט זה לשמור על תוכנו של דף אינטרנט במקומי. תוכניתנים עושים זאת לניתוח נתונים, בדיקות אוטומטיות ואיסוף מידע.

## איך לעשות:
להלן דוגמא להורדת דף אינטרנט בשפת Clojure באמצעות הספריה `clj-http`:

```Clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (let [response (client/get url)]
    (println (:status response))
    (:body response)))

(defn -main [& args]
  (println (download-page "http://example.com")))

(-main)
```

פלט דוגמא:
```
200
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## עיון נוסף:
ההורדות הראשונות של דפי אינטרנט היו באמצעות כלים כמו wget ו-cURL. ב-Clojure, ספריות כמו `clj-http` ו`http-kit` מבצעות זאת בקלות ונוחות. חלופה נפוצה היא להשתמש בספריה `jsoup` לשלילה ועיבוד HTML, מה שמאפשר שילוב של הורדה וניתוח נתונים.

## ראה גם:
- [clj-http GitHub repo](https://github.com/dakrone/clj-http)
- [http-kit GitHub repo](https://github.com/http-kit/http-kit)
- [jsoup: Java HTML Parser](https://jsoup.org/)