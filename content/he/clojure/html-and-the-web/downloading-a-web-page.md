---
date: 2024-01-20 17:43:36.772992-07:00
description: "\u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05D3\u05E3 \u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8 \u05D6\u05D4 \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E2\u05DC\
  \ \u05EA\u05D5\u05DB\u05E0\u05D5 \u05E9\u05DC \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8 \u05D1\u05DE\u05E7\u05D5\u05DE\u05D9. \u05EA\u05D5\u05DB\u05E0\
  \u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DC\u05E0\u05D9\u05EA\u05D5\u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D1\
  \u05D3\u05D9\u05E7\u05D5\u05EA \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\u05D5\u05EA\
  \ \u05D5\u05D0\u05D9\u05E1\u05D5\u05E3 \u05DE\u05D9\u05D3\u05E2."
lastmod: '2024-02-25T18:49:37.015161-07:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8 \u05D6\u05D4 \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E2\u05DC \u05EA\
  \u05D5\u05DB\u05E0\u05D5 \u05E9\u05DC \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8 \u05D1\u05DE\u05E7\u05D5\u05DE\u05D9. \u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\
  \u05E0\u05D9\u05EA\u05D5\u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D1\u05D3\
  \u05D9\u05E7\u05D5\u05EA \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\u05D5\u05EA\
  \ \u05D5\u05D0\u05D9\u05E1\u05D5\u05E3 \u05DE\u05D9\u05D3\u05E2."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
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
