---
title:                "הפיכת מחרוזת לאותיות גדולות"
html_title:           "Clojure: הפיכת מחרוזת לאותיות גדולות"
simple_title:         "הפיכת מחרוזת לאותיות גדולות"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה זה & למה?: 

האפשרות להפוך את האות הראשונה במחרוזת לאות גדולה מהווה בנייה בלתי נפרדת של תוכנה. מתכנתים משתמשים בכלי זה כדי להפוך מחרוזות מורכבות לקריאות ונגישות יותר, שימוש למשל בכותרות מזהה.

## כיצד ל:
נראה איך אפשר לה שווהגב את זה בעצמנו:

```Clojure
(defn capitalize [s]
	(str (clojure.string/upper-case (first s)) (clojure.string/lower-case (subs s 1))))
```
פלט לדוגמה:

```Clojure
(capitalize "hello world") ; "Hello world"
```
## בקעות:
הגדולתי של מחרוזת היא שטריק שמקורו בפיתוח התכנה בשפת C. למרות שמדובר בכלי שימושי, קיימים שימושים מתחרים וגישות שונות להכתבה. בשפת Clojure עצמה, אנחנו נתבן בעזרת הגישה שהצגנו, אשר מחזירה מחרוזת חדשה בעזרת שילוב פונקציות חלופיות.

## ראה גם:
[Clojure String Documentation](https://clojure.github.io/clojure/clojure.string-api.html)
פרטים נוספים אודות מבנה מחרוזות ב-Clojure ניתן למצוא ב- [Clojure.org](https://clojure.org/api/cheatsheet).