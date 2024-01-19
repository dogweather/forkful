---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

#מה ולמה?

השוואת שני תאריכים הוא תהליך בו משווים את ההפרש בין שני תאריכים. תכנתים עשויים לעשות זאת כדי לראות מה מספר הימים, החודשים או השנים בין שני תאריכים.

#איך לעשות:

נכתוב את הטעימות של הקוד שלנו בעזרת הספרייה של Clojure `clj-time`.

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])
(require '[clj-time.period :as p])

(defn days-between [d1 d2]
  (p/in-millis (t/interval (c/to-date-time d1) (c/to-date-time d2))))

(def d1 (t/date-time 2022 7 5))
(def d2 (t/date-time 2022 7 9))

(println (days-between d1 d2))  ; 86400000 
```

בדוגמא זו, חישבנו כמה ימים יש בין שני תאריכים.

#צלילה עמוקה:

Clojure השתמשה בJava DateTime APIים בעבר, אך רק באופן מוגבל. הספרייה `clj-time` התפתחה כדי לפשט את העבודה עם תאריכים ושעות בClojure. יתר על כך, `clj-time` מספקת תכלית לניתוח מחרוזות תאריך עבור פורמטים שונים. 

אלטרנטיבות אחרות ל `clj-time` כוללות את `java.time`  שמגיעה כחלק מ-JDK 1.8 ומעלה, כמו גם `Joda-Time`, שזו ספרייה מבוססת-Java. 

אתה צריך להיות מוודא כי התאריכים שאתה משווה מספקים לציפיות שלך. לדוגמה, אם אתה משווה תאריך בלי שעה לתאריך עם שעה, אז תקבל תוצאות לא צפויות.

#ראו גם:

1. [Clojure clj-time Documentation](https://github.com/clj-time/clj-time) - המקום הראשון ללכת אם אתה מחפש מידע מעמיק יותר על `clj-time`.

2. [Java 8 Date Time API](https://www.baeldung.com/java-8-date-time-intro) - מדריך ל-Date Time API של Java 8.

3. [Joda-Time Library](https://www.joda.org/joda-time/) - הדף הראשי של Joda-Time.