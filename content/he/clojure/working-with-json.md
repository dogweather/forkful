---
title:                "עובדים עם json"
html_title:           "Clojure: עובדים עם json"
simple_title:         "עובדים עם json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-json.md"
---

{{< edit_this_page >}}

מה ולמה?
כשמדברים על עבודה עם JSON בקלות, זה אומר שאנחנו מתקשרים עם מידע מבנה של אובייקטים בפורמט טקסטואלי קל לקריאה והבנה. פרמטרים כמו טיפוסים שונים ונתוני מידע יכולים להיות מיוצגים בפורמט הזה. מחברות כפי שמשוייכים לטבע ומורחש כמו למשל של כתובות אי-מייל יכולות להיות בפורמט זה, ומכאן נובע הטריק של עבודה עם JSON כדי לקלות ולהציג תוכן בקלות למשתמש.

כיצד ל
כדי לעבוד עם JSON בClojure, ניתן להשתמש בספריית `clojure.data.json`. התחלת מבנה JSON וקריאה נכונה לשימוש שלו באמצעות הפונקציה `json/write-str`. ניתן לקרוא מחתימות נתונים מתוך JSON באמצעות הפונקציה `json/read-str`.

```clojure
(def data {:name "John" :age 32 :address "123 Main St"})
; ניתן להמיר את הנתונים למחרוזת בפורמט JSON
(clojure.data.json/write-str data)
; המילון המקשר ניתן לקרוא מהמחרוזת באמצעות הפונקציה
(clojure.data.json/read-str "{\"name\":\"John\",\"age\":32,\"address\":\"123 Main St\"}")
```

כיוליה רחבים
ישנם מספר אלטרנטיבות לעבודה עם JSON בקלות, כמו למשל ספריית `clojure.edn` שמתאימה יותר לעבודה עם נתונים בעלי טיפוסים מסוג הנתונים המבוזרים. אבל באופן כללי, JSON נחשב לפתרון פופולרי ווידל במיוחד עם הפופולריות של האפליקציות הניידות ואינטרנט המתפרש. הרבה מהן משתמשות ב JSON כפתרון חילוף מידע מתקדם.

ראה גם
לפרטים נוספים על עבודה עם JSON והכלים השונים שניתן להשתמש בהם ניתן לקרוא כאן: https://clojure.org/reference/data_structures#_json. ופונקציונליות ווידאו נוספים על כיצד לעבוד עם פרמטרים JSON נמצאים בתיעוד הזה.