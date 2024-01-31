---
title:                "ניתוח HTML"
date:                  2024-01-20T15:31:12.689295-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"

category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?

לעשות פרסינג ל-HTML זה להמיר את הקוד של דף אינטרנט לנתונים שאפשר לקרוא ולעבד בשפת תכנות. תוכניתנים עושים את זה כדי לחלץ מידע, לאוטומט טפסים, ולעשות בדיקות אוטומטיות לאתרים.

## איך לעשות:

```Clojure
(require '[enlive.core :as enlive])

;; נטען את ה-HTML
(defn fetch-html [url]
  (-> (java.net.URL. url) .openStream slurp))

;; מחזיר את כל טקסטים מתגית מסוימת
(defn extract-texts [html tag]
  (map :content (enlive/select (enlive/html-resource (java.io.StringReader. html)) [tag])))

;; דוגמה לשימוש
(def html-sample (fetch-html "https://www.example.com"))
(println (extract-texts html-sample :p))
```

דוגמת פלט:

```
("טקסט של פסקה ראשונה" "טקסט של פסקה שנייה" ...)
```

## עומק הים:

פרסינג ל-HTML אינו פשוט כמו המרה טקסטואלית רגילה. HTML יכול להיות מורכב ולא תמיד תקני. במהלך השנים, ספריות רבות נוצרו למטרה זו. הן מנסות להיות קשוחות לשגיאות ולנהל מגוון רחב של קידודי HTML.

enlive, שראינו לעיל, היא ספרייה חזקה לפרסינג של HTML/XML ב-Clojure. היא מאפשרת לנו לבצע שאילתות ולשנות את ה-HTML בצורה מוכוונת נתונים. חלופות כוללות ספריות כמו Hickory או jsoup, אשר מספקות יכולות דומות בזהויות של דומיינים שונים של מידע.

כאשר מגיעים לבחירה של כלי פרסינג, חשוב לשקול גמישות מראש, ביצועים וקלות שימוש. בחירת הכלים הנכונים יכולה להבדיל בין קוד פשוט ואלגנטי לבין קוד מסורבל ומתסכל.

## ראה גם:

- [Enlive GitHub Repository](https://github.com/cgrand/enlive)
- [Hickory GitHub Repository](https://github.com/davidsantiago/hickory)
- [Jsoup: Java HTML Parser](https://jsoup.org/)
