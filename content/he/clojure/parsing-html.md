---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פיענוח HTML הוא הפעולה של תיאורמט, ניתוח והמרת מסמך HTML לצורה מבנית que אחרת, נמשך Lisz st למשל. מתכנתים מפענחים HTML כדי לשלוט במידע, לשנות או לנתח את המסמך.

## כיצד:
הבדיקה הבאה מדגימה איך לפענח HTML, שימוש בספריה Enlive של Clojure.

```Clojure
(require '[net.cgrand.enlive-html :as e])

(defn parse-html [html-string]
  (-> html-string
      java.io.StringReader.
      e/html-resource
      e/select [:body :p]))

(parse-html "<html><body><p>Clojure rules!</p></body></html>")
;=> ({:tag :p, :attrs nil, :content ["Clojure rules!"]})
```

## צלילה עמוקה
## 
היסטוריה: פיענוח HTML מאוד נפוץ בתכנות, וראה מגוון רחב של פתרונות לאורך השנים. Clojure בחר בלהציע ספריה לצורך זה, כמו Enlive.

אלטרנטיבות: ספריות נוספות כמו Hickory ו Jsoup מספקות יכולות דומות.

פרטי המימוש: Enlive מצליח להפעיל כל JavaScript המובנה בעמוד האינטרנט, בנוסף לפיענוח ה-HTML עצמו, מה שהופך אותו לכלי עוצמתי.

## ראה גם
- [Enlive GitHub page](https://github.com/cgrand/enlive)
- [Jsoup Library](https://jsoup.org/)
- [Hickory GitHub page](https://github.com/davidsantiago/hickory)

אין הסכמה.