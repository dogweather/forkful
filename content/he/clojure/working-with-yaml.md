---
title:                "עבודה עם YAML"
html_title:           "Clojure: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?

העבודה עם YAML היא דרך לארגן ולשכפל מידע על פני קבצים בפורמט קריא על ידי מכונה ואנושית כאחד. תוכנתנים עובדים עם YAML כדי ליצור קבצי תצורה ועבור קבצי תצורה תומכים.

## איך?

כדי לעבוד עם YAML ב- Clojure, ניתן להשתמש בספריית clj-yaml. הנה דוגמא קצרה שמראה איך לקרוא קובץ YAML ולהחזיר את נתוניו כמחרוזת:

```Clojure
(ns my-project.yaml-parser
  (:require [clj-yaml.core :refer [load-string]]))

(def yaml-data (load-string "
name: John
age: 30"))

(print yaml-data)
```

פלט:

```
{:name "John", :age 30}
```

## שוקעת לתוך עומקים

פורמט YAML נוצר לראשונה בשנת 2001 ומשמש בעיקר כפורמט תיאור למידע קבוע תוך שימוש בכתיבת קוד פשוטה. ל- YAML ישנם ממשקים לשפות תכנות אחרות, כולל גם Clojure. אחת האלטרנטיבות לפורמט הזה היא JSON ועוד שניים פופולריים בשם XML ו- INI.

## ראה גם

[ספריית clj-yaml](https://github.com/lancepantz/clj-yaml)