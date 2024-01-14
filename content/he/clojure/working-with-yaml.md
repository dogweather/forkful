---
title:                "Clojure: עובדים עם YAML"
simple_title:         "עובדים עם YAML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

# מדוע

כתיבת קוד עם שפת Clojure מלווה ביתרונות רבים, כולל דינמיות ופשטות בתחזוקה. YAML הוא שפת תיאור לנתונים הנפוצה במסמכי קוד ועד כמה שראינו בסדר יום יום. כתיבת קוד עם YAML יכול להיות נוחה כששואלים עלותים מתאימים בפינה.

# האיך לעשות

דוגמאות קוד ופלט נמצאות בפריסה תחת "```Clojure ... ```".

```Clojure
(require '[to-yaml.core :as yaml]) ; לטעינת הספרייה

; קוד הממיר נתונים לחבילת YAML
(yaml/to-yaml {:name "John Smith", :age 32, :city "Tel Aviv"})

; פלט: "name: John Smith
age: 32
city: Tel Aviv"
```

כעת, אם נעדכן את שדה הגיל בניתוחנו הבא, נקבל פלט עם הערך המעודכן:

```Clojure
(yaml/to-yaml {:name "John Smith", :age 33, :city "Tel Aviv"})

; פלט: "name: John Smith
age: 33
city: Tel Aviv"
```

אפשר לתת ערכים מסוגים שונים כמו מספרים, מחרוזות, רשימות, ועוד. ספריית YAML מאפשרת גם להמיר חזרה ממבנה YAML למבנה נתונים של Clojure.

# מעמקים

ישנם כמה אפשרויות להתאים נתונים בעזרת YAML. כשאנחנו משתמשים בדפוסים מוכרים, כגון מערך של רשימות ומאפיינים מקונות, יש אפשרות להשתמש בפונקציות מבניות כמו פונקציות כמו merge, prettify וכו'. עבור דרישות מיוחדות, ניתן לחבר תכונות משלנו באמצעות פונקציות מותאמות.

# ראו גם

- קוד המקור של ספריית YAML:
https://github.com/tizoc/to-yaml

- העמקות נוספים בכתב הבלוג המדורש בקובצים תחת test /.

- תיעוד נוסף על ספריית YAML:
https://tizoc.github.io/yaml-doc/