---
title:                "עבודה עם yaml"
html_title:           "Clojure: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

שלום וברוכים הבאים לכתבה על Clojure! במאמר הזה אני אלמד לכם כיצד לעבוד עם YAML באמצעות השפה הזאת המפורסמת. אתם תגלו שעבודה עם YAML היא פשוטה ומהנה מאוד, וזהו מספיק כדי שתרצו להתחיל לעבוד עם זה כעת!

## Why

למה לעסוק בעבודה עם YAML? בקלות ניתן ליצור קבצי YAML מבלי להשתמש במעבדי קוד מיוחדים, וזהו דבר שנוצר במקור בשביל להקל על עבודת הקוד שלכם.

## How To

כדאי להתחיל בקריאת קבצי YAML כדי שתהיה לכם תמונה רחבה יותר של הפורמט. לדוגמה:

```Clojure
;; קובץ YAML דוגמה:

שם: גל
גיל: 30
עיר: תל אביב
מקצוע: מתכנת
```

כדי שנוכל לעבוד עם YAML בקוד Clojure, נצטרך להתקין את הספרייה `yaml`, שאפשר לעשות באמצעות כמה פקודות פשוטות בטרמינל השליטה.

```Clojure
;; דוגמה להשתמש בספרייה YAML:

(require '[yaml.core :as yaml])

(def person-yaml
    (slurp "person.yaml")) ;;person.yaml היא הקובץ המופעל

(print person-yaml)

;; פלט יהיה:

;; "שם: גל\nגיל: 30\nעיר: תל אביב\nמקצוע: מתכנת"
```

כפי שאתם רואים, הספרייה `yaml` מאפשרת לנו לקרוא ולהתמיד בין קבצי YAML ובין קוד Clojure בקלות ובאופן נוח.

## Deep Dive

עכשיו שיש לנו מושג טוב יותר על כיצד לקרוא ולעבוד עם קבצי YAML בקוד Clojure, אפשר לצאת לדרך ולנסות להתמודד עם קובץ YAML מעצמנו.

כדי ליצור קובץ YAML, אנחנו נשתמש בפונקציה `dump`, שתאפשר לנו להמיר מידע מאוסף נתונים בקוד Clojure לפורמט YAML. לד