---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
השימוש בביטויים רגולריים מאפשר חיפוש והתאמה של טקסט על פי תבנית. תכנתים עושים זאת כדי למצוא, להחליף או לבדוק תוכן טקסט בצורה חכמה ויעילה.

## איך לעשות:
ב-Clojure, אנו משתמשים בביטויים רגולריים בעזרת סינטקסיס דומה ל-Java. זה כולל פונקציות כמו `re-find`, `re-seq` לחיפוש, ו `re-matches` לבדיקת התאמות.

```Clojure
; חיפוש התאמה יחידה
(def pattern #"\b[Cc]lojure\b")
(re-find pattern "Clojure is awesome!") ; => "Clojure"

; מציאת כל ההתאמות
(re-seq pattern "Clojure and clojure are both cool.") ; => ("Clojure" "clojure")

; בדיקת התאמה מלאה
(re-matches pattern "Clojure") ; => "Clojure"
(re-matches pattern "I love Clojure") ; => nil
```

## צלילה לעומק:
ביטויים רגולריים הם חלק מתכנות כמעט מהימים הראשונים שלו. במקור פותחו על ידי תאורטיקנים של מדעי המחשב כמו Ken Thompson. חלופות כוללות פרסור סינטקסי מובנית, אולם ביטויים רגולריים עדיין נשארים פופולריים בגלל הגמישות והעוצמה שלהם. בקרבת, יעילות אלגוריתמים כמו סיבוכיות זמן עבודה הינה נושא חשוב.

## ראה גם:
- [ClojureDocs - Regular Expressions](https://clojuredocs.org/clojure.core/re-find)
- [Mozilla Developer Network - Regular Expressions Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
