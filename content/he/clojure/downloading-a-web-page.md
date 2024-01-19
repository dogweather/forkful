---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?

אימצאט ויב של האינטרנט הוא לשם החזרה של דף אינטרנט ללקוח. תכנתים עושים בדיוק אותו דבר, אבל ישירות מתוך קוד התוכנית שלהם. הם מבצעים את זה לשם איסוף מידע, ניתוח נתונים או יצירת אינטראקציה עם אתרים אחרים שונים.

## כיצד לעשות:

```Clojure
(ns downloader.core
  (:require [clj-http.client :as client]))

(defn download-page
  [url]
  (:body (client/get url {:as :string})))
```

הפונקציה הזו תוריד דף אינטרנט פשוט. לדוגמה:

```Clojure
(download-page "http://www.example.com")
```

אתה צפוי לראות את תוכן הדף שורש של http://www.example.com מודפס לקונסולה.

## בעומק:

1. הדינמיקה של האינטרנט: בפעם הראשונה, האינטרנט היה רק טקסט פשוט. למרבה המזל, כיום, HTTP יכול לבצע הרבה יותר.
2. חלופות: קיימות שפות אחרות אתה יכול לעשות זאת, למשל Python עם BeautifulSoup או JavaScript עם Cheerio.
3. פרטי היישום: בסביבת העבודה שלך, אתה עשוי להשתמש בתוספות אחרות כדי להרחיב את פונקציונליות Clj-http, כמו ה- :follow-redirects או :coercion.

## ראה גם:

- Clojure עבור המתחיל הזהיר -- http://www.braveclojure.com/clojure-for-the-brave-and-true/
- קוד מדריך עבור Clj-http client -- https://github.com/dakrone/clj-http
- Cheerio (JavaScript) -- https://cheerio.js.org/
- BeautifulSoup (ב-Python) -- https://www.crummy.com/software/BeautifulSoup/bs4/doc/