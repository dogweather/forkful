---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:00.801329-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Clojure \u05D0\
  \u05D9\u05DF \u05D9\u05DB\u05D5\u05DC\u05D5\u05EA \u05E4\u05E2\u05E0\u05D5\u05D7\
  \ HTML \u05DE\u05D5\u05D1\u05E0\u05D5\u05EA, \u05D0\u05DA \u05E0\u05D9\u05EA\u05DF\
  \ \u05DC\u05E0\u05E6\u05DC \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E9\u05DC Java\
  \ \u05D0\u05D5 \u05DE\u05E2\u05D8\u05E4\u05D5\u05EA \u05E9\u05DC Clojure \u05DB\u05D2\
  \u05D5\u05DF `enlive` \u05D0\u05D5 `hickory`. \u05D4\u05E0\u05D4 \u05DB\u05D9\u05E6\
  \u05D3 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E9\u05E0\u05D9\u05D4\u05DD\
  ."
lastmod: '2024-04-05T21:53:40.013473-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Clojure \u05D0\u05D9\u05DF \u05D9\u05DB\u05D5\u05DC\u05D5\u05EA \u05E4\
  \u05E2\u05E0\u05D5\u05D7 HTML \u05DE\u05D5\u05D1\u05E0\u05D5\u05EA, \u05D0\u05DA\
  \ \u05E0\u05D9\u05EA\u05DF \u05DC\u05E0\u05E6\u05DC \u05E1\u05E4\u05E8\u05D9\u05D5\
  \u05EA \u05E9\u05DC Java \u05D0\u05D5 \u05DE\u05E2\u05D8\u05E4\u05D5\u05EA \u05E9\
  \u05DC Clojure \u05DB\u05D2\u05D5\u05DF `enlive` \u05D0\u05D5 `hickory`."
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
weight: 43
---

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
