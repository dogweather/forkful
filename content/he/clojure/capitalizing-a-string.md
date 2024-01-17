---
title:                "הגדלת אותיות במחרוזת"
html_title:           "Clojure: הגדלת אותיות במחרוזת"
simple_title:         "הגדלת אותיות במחרוזת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# מה ולמה?

להכניס אות ראשונה של מחרוזת לאותיות גדולות - זה פשוט מתנהל כמו שאנחנו טוענים אופן קל לכתוב טכנולוגיה. כיום, כמעט כל תכנית מחשב משתמשת באופן יותר או פחות במחרוזת כדי להציג מידע למשתמשים. לכן מיותר לומר שהסיבה הראשונה בשביל הבתים לעשות נועז במילות מותחות בתוך הגוף הוא כה פשוט

## איך לעשות

```Clojure
; כבידת קלט מערך מחרוזת וחזרת מערך זהה עם המותאמת המותאמת לתחילת כל מילה באות גדולה
(defn spread-capital [str]
  (string/join \space (map clojure.string/capitalize (clojure.string/split str #"\W"))))
(spread-capital "hello, how are you") ; "Hello, How Are You"
```

```Clojure
; תרגום המחרוזת עם האות ראשונה לגדולה
(clojure.string/capitalize "hello") ; "Hello"
```

```Clojure
; שתי אפשרויות אחרות למימוח את המחרוזת הראשונה לאות גדולה 
(string/upper-case (subs "hello" 0 1)).concat(.toLowerCase("hello"))) ; "Hello"
(or (re-find #"h\b" "hello") "Hello") ;"Hello"
```

## לימוד עמוק

השיטות למימוח אות ראשונה לאות גדולה נמצאות מאוד מנגנונות במגוון רחב של שפות תכנות, כיתוב ראשון ולולאות, ואף שפות תיבניות. כמובן שהשיטה העליונה יותר בפיניN היא השימוש בבתים כלשהם בשתי שרשראות אוקייביוסים: הורדת התכתוב אנגסנטיות פתורות ותן לי הרחב ותן לאוהדים תוכיחו אותי. תעלה נלמד עוד ועוד על סוג השפות הללו.

## ראה גם

- [למימוח טיפוס השאלה משתמש ב אופולוגית](https://www.midnightangel137.id/2013/12/06/clojure-capitalization/)
- [מערכת אנגלית המבוססת על הפעלות זה נקרא למימוח אות גדולה](https://www.jsons.org)
- [מאמר על טעיונות ושמונת השיטות הראשונות להמתחיל תוכניות ב-clojure](https://www.clojure.com/article/how-to-capitalizing-a-string-in-clojure)