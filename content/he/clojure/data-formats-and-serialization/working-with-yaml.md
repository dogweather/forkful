---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:12.148276-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Clojure \u05D0\u05D9\
  \u05E0\u05D4 \u05DB\u05D5\u05DC\u05DC\u05EA \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\
  \u05D5\u05D1\u05E0\u05D9\u05EA \u05DC-YAML, \u05D0\u05DA \u05E0\u05D9\u05EA\u05DF\
  \ \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA\
  \ \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DB\u05D2\u05D5\u05DF `clj-yaml`\
  \ \u05DC\u05E0\u05D9\u05EA\u05D5\u05D7 \u05D5\u05D9\u05D9\u05E6\u05D5\u05E8 \u05E0\
  \u05EA\u05D5\u05E0\u05D9 YAML. \u05EA\u05D7\u05D9\u05DC\u05D4, \u05D4\u05D5\u05E1\
  \u05E3 \u05D0\u05EA \u05D4\u05E1\u05E4\u05E8\u05D9\u05D4 \u05DC\u05EA\u05DC\u05D5\
  \u05EA\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.737870-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u05D0\u05D9\u05E0\u05D4 \u05DB\u05D5\u05DC\u05DC\u05EA \u05EA\u05DE\
  \u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC-YAML, \u05D0\u05DA\
  \ \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\
  \u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DB\u05D2\
  \u05D5\u05DF `clj-yaml` \u05DC\u05E0\u05D9\u05EA\u05D5\u05D7 \u05D5\u05D9\u05D9\u05E6\
  \u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9 YAML."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## איך לעשות:
Clojure אינה כוללת תמיכה מובנית ל-YAML, אך ניתן להשתמש בספריות צד שלישי כגון `clj-yaml` לניתוח וייצור נתוני YAML. תחילה, הוסף את הספריה לתלותות הפרוייקט שלך:

```clojure
;; הוסף זאת לתלותות של project.clj
[clj-yaml "0.7.0"]
```

הנה כיצד ניתן להשתמש ב-`clj-yaml` לניתוח YAML והמרת מפות Clojure ל-YAML.

### ניתוח YAML:
```clojure
(require '[clj-yaml.core :as yaml])

;; ניתוח מחרוזת YAML
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; פלט:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### ייצור YAML מ-Clojure:
```clojure
(require '[clj-yaml.core :as yaml])

;; המרת מפת Clojure למחרוזת YAML
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; פלט:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

הפעולות הפשוטות האלה עם `clj-yaml` ניתנות לשילוב באפליקציות Clojure לצורך טיפול בקבצי תצורה או קידום תהליכי החלפת נתונים עם שירותים או רכיבים אחרים המשתמשים ב-YAML.
