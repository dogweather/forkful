---
title:                "פירוק של html"
html_title:           "Clojure: פירוק של html"
simple_title:         "פירוק של html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?

הניתוח של HTML הוא תהליך שמאפשר לקודמים לקרוא ולהבין את קודים האתרים באופן ממוחשב. ניתוח זה משמש כדי להפוך את תוכן האתר לנתונים מבניים יותר נגישים ונוחים לעיבוד על ידי תוכניות תכנות כגון Clojure.

## כיצד ל:

כדי להפעיל את ניתוח ה-HTML בתכנית Clojure שלך, ניתן להשתמש בפונקציות של מודול Clojure "hiccup" כדי לטפל בקידוד של ה-HTML. לדוגמה, הנה פקודות שיכולות לעזור לך לקרוא את קוד ה-HTML ולהדפיס את התוכן המבני שלו:

```
;; הגדרת קוד HTML
(def my-html "<div><h1>Hello World!</h1></div>")

;; שימוש בפונקציות של hiccup
(require '[hiccup.core :as hiccup])

;; ניתוח קוד ה-HTML והדפסת התוכן המבני
(println (hiccup/html-to-str my-html))
```

פלט הקוד הזה יהיה:

```
<h1>Hello World!</h1>
```

## טיול עמוק:

הניתוח של HTML נחשב למקורי ולא יעיל, כך שתכונת המודול "hiccup" היא שמייצרת קוד HTML באופן יעיל וקריא. ישנם גם תוכניות תכנות אחרות כמו "Enlive" ו"Jericho" המשמשות לטיפול וניתוח גמישים יותר של קודי HTML.

עוד פופולריות יתר של פתרונות לניתוח ה-HTML כוללים שימוש בתוכנת ה-Java "JSoup" או בספרייה של ניתוח הנתונים החדשה "ClojureQL".

מבחינה מבנית, האלגוריתם של ניתוח ה-HTML מדרג כל קוד HTML לתצורה עץ של תגיות ותוכן, וכך מאפשר לתכניות לקרוא ולעבד את תוכן האתר בקלות.

## ראו גם:

- ניתוח קוד HTML עם Clojure "Enlive": https://github.com/cgrand/enlive
- ניתוח קוד HTML עם ClojureQL: https://github.com/LauJensen/clojureql
- תיעוד על תוכניות תקשורת כגון "JSoup" ו-"Jericho": https://github.com/clojure-cookbook/clojure-cookbook/tree/master/02_advjava/2-12_html-parsing