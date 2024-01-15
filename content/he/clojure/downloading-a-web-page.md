---
title:                "הורדת עמוד אינטרנט"
html_title:           "Clojure: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Hebrew:

## למה
אנשים בכל רחבי העולם מתחברים לאינטרנט בכדי לגלות מידע חדש או ליצור תמונת מציאות חדשה על עולמם. צריכת דפי אינטרנט היא דרך מהירה ויעילה ביותר לעשות זאת, ולכן ניתן להבין למה מישהו ירצה להוריד דף אינטרנט.

## איך לעשות זאת
הנה מספר דוגמאות לקוד ולפלט של דפי אינטרנט שניתן להוריד באמצעות קלוז'ור:

```Clojure
;להוריד דף אינטרנט ולשמור אותו בקובץ
(require '[clojure.java.io :as io])
(io/copy (io/input-stream "http://www.example.com") (io/file "example.html"))

;להוריד דף אינטרנט ולהדפיס את הקישורים בו
(require '[clojure.contrib.duck-streams :as dts])
(dts/each-line (io/reader (java.net.URL. "http://www.example.com"))
  #(println (re-seq #"https?://[a-zA-Z0-9./?#-_=]+"
                     (java.util.regex.Matcher. "%" %))))

;להוריד דף אינטרנט עם ביצוע של HTTP בקשת GET ולהציג את התגית הראשית של הדף
(require '[org.httpkit.client :as http])
(http/get "https://www.example.com"
          {:logger true
           :debug-body "single response"
           :throw-exceptions false}
          (fn [resp]
            (org.jsoup.Jsoup/parse resp)))

```

## מעמקים
ישנם מספר סיבות למה צריך להוריד דפי אינטרנט כאשר משתמשים בקלוז'ור. למשל, ניתן להשתמש בזה כדי לגבות תוכן אינטרנטי או ליצור ממשק גרפי לאינטרנט על ידי איסוף נתונים מאתרים שונים. למעשה, פעולת צריכת דפי אינטרנט היא חלק חיוני מהעבודה של כל מפתח קלוז'ור והיא מאפשרת שימוש מרחבי בכל חלקי האינטרנט.

## ראו גם
- טכניקות נוספות לצריכת דפי אינטרנט בקלוז'ור: https://www.douzi.co.il/feed/html/web-scraping-with-clojure/
- הדרכה מפורטת י