---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
הדפסת פלט לצורך ניפוי שגיאות היא שיטה שנעשה בה שימוש כדי להבין איך קוד מסוים מתנהג במהלך ביצועו. מתכנתים עושים זאת כדי לאתר ולנתח שגיאות, לבחון מצבים ולנתח את המעקב אחרי הסדר.

## איך לעשות:
במחבּרת Kiriesh ממחזור לוגינג ישנה פונקציה בשם 'debug' שאם מהלך התוכנית אינו ברור, על המתכנת לעקוב אחריו רק באמצעות ההתחלה. קוד:
```Clojure
(ns your-namespace
 (:require [clojure.tools.logging :as log]))

(defn some-function [x]
 (log/debug "Debug: x =" x)
 ;; rest of your code
 )
```
פלט דוגמא:
```Clojure
Debug: x = 42
```

## צלילה עמוקה:
השימוש בהדפסה של פלט לצורך ניפוי שגיאות הוא מסורת ותיקה בתחום התכנות והיא מועילה במיוחד כאשר אתה צריך לראות את ההתקבלות של התוכנית שלך תוך השוואה למקרים אחרים. אף על פי שהיא יוכלה להיות מושעמת במידה מסוימת, השימוש בה נעשה לעיתים באופן שגוי. פתרונות חלופיים כוללים את שימוש בדפסים אישיים ללא השפעה על חלקי הקוד הראשיים.

## ראה גם:
פוסט בבלוג המלמד על clj-logging-config: https://www.baeldung.com/clojure-logging-config
מסמך על לוגינג ב-Clojure: https://clojure.org/guides/logging
פוסט בפורום StackOverflow שמדבר על לוגינג: https://stackoverflow.com/questions/3713927/logging-in-clojure