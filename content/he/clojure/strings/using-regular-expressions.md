---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:11.463153-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Clojure, \u05E0\u05D0\
  \u05DE\u05DF \u05DC\u05E9\u05D5\u05E8\u05E9\u05D9\u05D5 \u05D1\u05DE\u05E9\u05E4\
  \u05D7\u05EA Lisp, \u05DE\u05E6\u05D9\u05E2 \u05DE\u05E2\u05E8\u05DA \u05E2\u05E9\
  \u05D9\u05E8 \u05E9\u05DC \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05D4\
  \u05DE\u05EA\u05DE\u05DE\u05E9\u05E7\u05D5\u05EA \u05D1\u05D7\u05DC\u05E7\u05D5\u05EA\
  \ \u05E2\u05DD \u05D9\u05DB\u05D5\u05DC\u05D5\u05EA \u05D4\u05D1\u05D9\u05D8\u05D5\
  \u05D9 \u05D4\u05E8\u05D2\u05D5\u05DC\u05E8\u05D9 \u05E9\u05DC Java. \u05D4\u05E0\
  \u05D4 \u05D0\u05D9\u05DA \u05D0\u05E4\u05E9\u05E8 \u05DC\u05E0\u05E6\u05DC \u05D0\
  \u05D5\u05EA\u05DD."
lastmod: '2024-04-05T21:53:40.003316-06:00'
model: gpt-4-0125-preview
summary: "Clojure, \u05E0\u05D0\u05DE\u05DF \u05DC\u05E9\u05D5\u05E8\u05E9\u05D9\u05D5\
  \ \u05D1\u05DE\u05E9\u05E4\u05D7\u05EA Lisp, \u05DE\u05E6\u05D9\u05E2 \u05DE\u05E2\
  \u05E8\u05DA \u05E2\u05E9\u05D9\u05E8 \u05E9\u05DC \u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D5\u05EA \u05D4\u05DE\u05EA\u05DE\u05DE\u05E9\u05E7\u05D5\u05EA \u05D1\
  \u05D7\u05DC\u05E7\u05D5\u05EA \u05E2\u05DD \u05D9\u05DB\u05D5\u05DC\u05D5\u05EA\
  \ \u05D4\u05D1\u05D9\u05D8\u05D5\u05D9 \u05D4\u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\
  \ \u05E9\u05DC Java."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
weight: 11
---

## איך לעשות:
Clojure, נאמן לשורשיו במשפחת Lisp, מציע מערך עשיר של פונקציות המתממשקות בחלקות עם יכולות הביטוי הרגולרי של Java. הנה איך אפשר לנצל אותם:

### התאמה בסיסית
כדי לבדוק אם מחרוזת מתאימה לתבנית, השתמשו ב- `re-matches`. היא מחזירה את ההתאמה המלאה אם הצליחה או `nil` אחרת.

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### חיפוש תבניות
כדי למצוא את הופעת התבנית הראשונה, `re-find` היא הפונקציה אליה תפנו:

```clojure
(re-find #"\d+" "Order 123")  ;=> "123"
```

### קיבוץ תפיסות
השתמשו ב- `re-find` יחד עם סוגריים בתבנית כדי לתפוס קבוצות:

```clojure
(let [[_ area code] (re-find #"(1)?(\d{3})" "Phone: 123-4567")]
  (println "Area Code:" area "Code:" code))
;; פלט: Area Code: nil Code: 123
```

### חיפוש גלובלי (מציאת כל ההתאמות)
ב-Clojure אין חיפוש גלובלי מובנה כמו בחלק מהשפות. במקום זאת, השתמשו ב- `re-seq` כדי לקבל רצף עצלן של כל ההתאמות:

```clojure
(re-seq #"\d+" "id: 123, qty: 456")  ;=> ("123" "456")
```

### פיצול מחרוזות
כדי לפצל מחרוזת בהתבסס על תבנית, השתמשו ב- `clojure.string/split`:

```clojure
(clojure.string/split "John,Doe,30" #",")  ;=> ["John" "Doe" "30"]
```

### החלפה
החליפו חלקים ממחרוזת שמתאימים לתבנית עם `clojure.string/replace`:

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "YYYY")  ;=> "YYYY-04-01"
```

### ספריות צד שלישי
למרות שהתמיכה המובנית של Clojure מספיקה לרוב המקרים, לסיטואציות מורכבות יותר, שקלו שימוש בספריות כמו `clojure.spec` לאימות נתונים תקף ו-`reagent` לניהול DOM ריאקטיבי ביישומי אינטרנט עם ניתוב ואימות קלט מבוססי ביטויים רגולריים.

```clojure
;; דוגמה של שימוש ב-clojure.spec לאימות דואר אלקטרוני
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valid? ::email "test@example.com")  ;=> נכון
```

זכרו, למרות שביטויים רגולריים הם כלי עוצמתי, הם גם יכולים להפוך קוד לקשה לקריאה ולתחזוקה. השתמשו בהם בשיקול דעת ותמיד שקלו פונקציות פשוטות יותר של ניהול מחרוזות ככל הניתן.
