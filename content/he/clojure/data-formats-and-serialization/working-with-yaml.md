---
title:                "עבודה עם YAML"
aliases:
- /he/clojure/working-with-yaml/
date:                  2024-02-03T19:25:12.148276-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

YAML, ראשי תיבות רקורסיביים ל-"YAML Ain't Markup Language", הוא פורמט סידוריות נתונים קריא לאדם המשמש לקבצי תצורה ולהחלפת נתונים בין שפות עם מבני נתונים שונים. מתכנתים מנצלים את YAML בשל פשטותו וקריאותו, הופכים אותו לבחירה אידיאלית לצורך תצורת אפליקציות וקידום תהליכי החלפת נתונים בסביבות תכנות פוליגלוטיות.

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
