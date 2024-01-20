---
title:                "עבודה עם YAML"
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט נתונים לקונפיגורציה, פשוט לקריאה וכתיבה. תוכניתנים משתמשים ב-YAML כדי לטעון, לשנות ולשמור הגדרות באפליקציות.

## איך לעשות:
קודם כל, תוסיפו את תלות `yaml` בקובץ `project.clj`:
```clojure
[clj-yaml "0.7.0"]
```
אז, תוכלו לטעון קובץ YAML כך:
```clojure
(require '[clj-yaml.core :as yaml])

(def config (yaml/load-string "
a: 1
b: 2
c:
  - 3
  - 4"))

(println config)
```
הרצה תפיק:
```clojure
{:a 1, :b 2, :c [3 4]}
```
שמירת נתונים ל-YAML:
```clojure
(def data-to-save {:name "Yonatan" :age 30 :languages ["Clojure" "JavaScript"]})

(spit "output.yaml" (yaml/generate-string data-to-save))
```
זה יכתוב בקובץ `output.yaml`.

## טבילה עמוקה
YAML (YAML Ain't Markup Language) באמת אינו שפת הציון אלא פורמט המנגנון של נתונים, יצא לראשונה ב-2001. חלופות פופולריות הן JSON ו-XML ששונות בעיקר בקריאותיות שלהן ובדרך כתיבת הסינטקס. בשימוש ב-YAML ב-Clojure, clj-yaml עוטף את ספריית SnakeYAML של Java, מה שמאפשר שילוב חלק עם עולם ה-JVM.

## ראו גם:
- הפרויקט `clj-yaml` ב-GitHub: https://github.com/clj-commons/clj-yaml
- מסמך המפרט של YAML: https://yaml.org/spec/
- טוטוריאל ל-Clojure למתחילים: https://www.learn-clojure.com/
- השוואה בין פורמטים שונים (JSON, YAML, XML): https://blog.logrocket.com/json-vs-xml-vs-yaml-comparison/