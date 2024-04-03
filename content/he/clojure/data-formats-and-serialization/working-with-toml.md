---
date: 2024-01-26 04:21:04.377288-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D5\
  \u05D3 \u05E2\u05DD TOML \u05D1- Clojure, \u05D0\u05EA\u05DD \u05D6\u05E7\u05D5\u05E7\
  \u05D9\u05DD \u05DC\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05DB\u05DE\u05D5 `clj-toml`.\
  \ \u05E7\u05D5\u05D3\u05DD \u05DB\u05DC, \u05D4\u05D5\u05E1\u05D9\u05E4\u05D5 \u05D0\
  \u05D5\u05EA\u05D4 \u05DC- `deps.edn` \u05E9\u05DC\u05DB\u05DD."
lastmod: '2024-03-13T22:44:38.742573-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD TOML \u05D1\
  - Clojure, \u05D0\u05EA\u05DD \u05D6\u05E7\u05D5\u05E7\u05D9\u05DD \u05DC\u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05D4 \u05DB\u05DE\u05D5 `clj-toml`."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
weight: 39
---

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
