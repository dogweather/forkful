---
title:                "פיענוח HTML"
aliases:
- /he/clojure/parsing-html.md
date:                  2024-02-03T19:12:00.801329-07:00
model:                 gpt-4-0125-preview
simple_title:         "פיענוח HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

פענוח HTML ב-Clojure כולל שליפה תכנותית של מידע ממסמכי HTML. מתכנתים עושים זאת כדי לגשת, לנהל או לנטר תכני אינטרנט דינאמית, מאוטמת של משימות או הזנת נתונים לאפליקציות.

## איך לעשות:

ב-Clojure אין יכולות פענוח HTML מובנות, אך ניתן לנצל ספריות של Java או מעטפות של Clojure כגון `enlive` או `hickory`. הנה כיצד להשתמש בשניהם:

### באמצעות Enlive:

Enlive הוא בחירה פופולרית לפענוח HTML ולגריפת אתרים. ראשית, כלול אותו בתלותיות הפרויקט שלך:

```clojure
[net.cgrand/enlive "1.1.6"]
```

לאחר מכן, תוכל לפרסר ולנווט ב-HTML כך:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

קטע זה משיג דף HTML ובוחר את כל אלמנטי ה-`<div>` עם המחלקה `some-class`.

הפלט עשוי להיראות כך:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["הנה קצת תוכן."]})
```

### באמצעות Hickory:

Hickory מספק דרך לפענח HTML לפורמט שנוח יותר לעבוד איתו ב-Clojure. הוסף את Hickory לתלותיות הפרויקט שלך:

```clojure
[hickory "0.7.1"]
```

הנה דוגמה פשוטה:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; פרסור ה-HTML לפורמט Hickory
(let [doc (hickory/parse "<html><body><div id='main'>שלום, עולם!</div></body></html>")]
  ;; בחירת ה-div עם ה-id 'main'
  (select/select (select/id "main") doc))
```

קוד זה מפרסר מחרוזת HTML פשוטה ומשתמש בבורר CSS כדי למצוא `div` עם ה-ID `main`.

דוגמה לפלט:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["שלום, עולם!"]}]
```

גם `enlive` וגם `hickory` מציעים פתרונות מוצקים לפענוח HTML ב-Clojure, כש-`enlive` מתמקד יותר בתבניות ו-`hickory` מדגיש המרה של נתונים.
