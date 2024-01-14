---
title:                "Clojure: שימוש ראשוני במחרוזת"
simple_title:         "שימוש ראשוני במחרוזת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה
כתיבת פונקציה המקפיצה (capitalizing) מחרוזת היא נפוצה בתחום התכנות הפונקציונלי של קלוז'ר (Clojure). כתבו כאן 1-2 משפטים סימפלים שמסבירים לקוראים למה בכלל מפעילים פעולה כזו.

## איך לעשות
נדגים כאן דוגמאות של קוד קלוז'ר (Clojure) הכוללת מקפיצת מחרוזת תוך שימוש בפונקציה `clojure.string/capitalize`, וכן את הפלט (output) של הפונקציה.
```Clojure
(defn capitalize-string [text]
  (clojure.string/capitalize text))

(capitalize-string "שלום עולם") ; מחזיר "שלום עולם"

(capitalize-string "hello world") ; מחזיר "Hello world"
```

## יירוד לפרטים
הפונקציה `clojure.string/capitalize` מחזירה את המחרוזת המקורית עם התחילית כתובה באות גדולה. אם התחילית כבר הייתה כתובה באות גדולה, הפעולה לא תשנה כלום. בנוסף, הפונקציה תתמוך בכל התחיליות המוקריות וההצגה המוחלטת של האופניים הבינלאומיות.

## ראה גם
למידע נוסף על הפונקציה `clojure.string/capitalize` ועל תכנות בקלוז'ר (Clojure) בכלל, תוכלו לעיין בקישורים הבאים:
- [Clojure Docs](https://clojuredocs.org/clojure.string/capitalize) - מדריך מפורט על הפונקציה `clojure.string/capitalize`
- [מדריך בעברית לתכנות בקלוז'ר](https://www.eval.co.il/קוד-פתוח/תכנות-בקלוז'ר/) - מדריך מפרט ומעודכן על קלוז'ר (Clojure) בעברית
- [מדריך אינטראקטיבי לקלוז'ר](https://clojurecademy.com/) - אתר מכליל שלבים התואמים לכל הרמות ידע למי שרוצה ללמוד קלוז'ר (Clojure) בצורה אינטראקטיבית.