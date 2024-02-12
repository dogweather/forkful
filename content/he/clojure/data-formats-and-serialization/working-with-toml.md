---
title:                "עבודה עם TOML"
aliases:
- /he/clojure/working-with-toml.md
date:                  2024-01-26T04:21:04.377288-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-toml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם TOML אומרת שאתם מטפלים בנתונים בפורמט מינימלי "Tom's Obvious, Minimal Language", פופולרי לקבצי תצורה בזכות קריאותו הקלה. תכנתים משתמשים בו לניהול תצורה ישיר ופשוט שעובד ישר מהקופסה עם תחביר נוח לבני אדם.

## איך ל:
כדי לעבוד עם TOML ב- Clojure, אתם זקוקים לספרייה כמו `clj-toml`. קודם כל, הוסיפו אותה ל- `deps.edn` שלכם:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

לאחר מכן, נתחו קצת TOML:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'TOML Example'")

(def parsed-config (toml/parse-string config-str))

;; להשיג את הכותרת מה-TOML שנתח
(println (:title parsed-config)) ;; הפלט: TOML Example
```

לייצר TOML:

```clojure
(def data {:title "TOML Example"})

(println (toml/generate-string data))
;; הפלט: title = "TOML Example"
```

## צלילה עמוקה
TOML נוצר בערך ב-2013 על ידי טום פרסטון-וורנר, שותף מייסד של GitHub, כחלופה פשוטה יותר ל-YAML ו-JSON לקבצי תצורה. הוא שואף לבהירות ומטרתו להיות מפרט שבני אדם יכולים לקרוא בלי כלים נוספים.

בעוד ש-JSON לעיתים קרובות משמש ל-APIs ולאפליקציות אינטרנט, ו-YAML יכול להיות מורכב עם הפניות ויכולות סקריפטינג, TOML בולט במיקוד על מבנים פשוטים מבוססי טבלאות. הפשטות הזאת הופכת אותו לפופולרי במיוחד בקהילת Rust ובסביבות שפות מודרניות אחרות.

Clojure, עם המיקוד שלה על פשטות ומעשיות, משתלבת היטב עם TOML לתצורה. `clj-toml` או ספריות חלופיות מגשרות על הפער. הן מתרגמות את נתוני ה-TOML הסטטיים לעולמה הדינמי והפונקציונלי של Clojure.

## ראו גם
- מאגר ה-GitHub של TOML: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` ב-Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- מסמכי Clojure: [clojure.org](https://clojure.org/guides/getting_started)
- היכרות עם `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
