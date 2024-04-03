---
date: 2024-01-20 17:42:06.357258-07:00
description: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\
  \u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05D3\u05E4\u05D5\u05E1 \u05D4\u05D9\u05D0\
  \ \u05DC\u05E7\u05D7\u05EA \u05D8\u05E7\u05E1\u05D8 \u05D5\u05DC\u05E9\u05DC\u05D5\
  \u05DC \u05DE\u05DE\u05E0\u05D5 \u05DB\u05DC \u05D7\u05DC\u05E7 \u05E9\u05DE\u05EA\
  \u05D0\u05D9\u05DD \u05DC\u05DE\u05D5\u05D3\u05DC \u05DE\u05E1\u05D5\u05D9\u05DD\
  . \u05EA\u05DB\u05E0\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D0\u05E9\u05E8 \u05D4\u05DD \u05E8\u05D5\u05E6\u05D9\
  \u05DD \u05DC\u05E0\u05E7\u05D5\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D0\
  \u05D5 \u05DC\u05D4\u05E4\u05D5\u05DA \u05D0\u05D5\u05EA\u05DD \u05DC\u05D0\u05D7\
  \u05D9\u05D3\u05D9\u05DD \u05D9\u05D5\u05EA\u05E8."
lastmod: '2024-03-13T22:44:38.678625-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05D3\u05E4\u05D5\u05E1 \u05D4\u05D9\u05D0 \u05DC\
  \u05E7\u05D7\u05EA \u05D8\u05E7\u05E1\u05D8 \u05D5\u05DC\u05E9\u05DC\u05D5\u05DC\
  \ \u05DE\u05DE\u05E0\u05D5 \u05DB\u05DC \u05D7\u05DC\u05E7 \u05E9\u05DE\u05EA\u05D0\
  \u05D9\u05DD \u05DC\u05DE\u05D5\u05D3\u05DC \u05DE\u05E1\u05D5\u05D9\u05DD."
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
weight: 5
---

## איך לעשות:
הנה דוגמה למחיקת תווים באמצעות RegExp ב-Clojure:

```Clojure
(re-seq #"[^\d]" "abc123!")  ; שומר רק על המספרים
;; => ("a" "b" "c" "!")
```

אולי תרצו גם להסיר רווחים, נקודות או תווים מיוחדים:

```Clojure
(clojure.string/replace "Hello, World! 123." #"[^\w\s]" "")
;; => "Hello World 123"
```

מחקת אותיות מיוחדות כמו סימני פיסוק, נקודות ופסיקים:

```Clojure
(clojure.string/replace "This is a test. Do not panic!" #"[.,!]" "")
;; => "This is a test Do not panic"
```

## עיון מעמיק
מחיקת תווים שתואמים לדפוס היא רעיון ישן כמעט כמו התכנות עצמו. מבוסס על ביטויים רגולריים (Regular Expressions), שהיו חלק מהפקד של כלי UNIX מהשנות ה-70, כמו sed או grep. ב-Clojure משתמשים בביטויים רגולריים כדי לעבוד עם טקסט בצורה חזקה וגמישה. ניתן להפעיל שינויים על מחרוזות בכמה שורות פשוטות של קוד.

אלטרנטיבות לביטויים רגולריים כוללות פונקציות מחרוזת מובנות, כמו `.indexOf`, `.substring` או `.split`, אבל אלה לא תמיד יעילות כמו ביטוי רגולרי מורכב. 

לגבי פרטי היישום, ב-Clojure, `clojure.string/replace` היא הפונקציה הנפוצה למחיקת תווים. היא מקבלת מחרוזת, פטרן (ביטוי רגולרי) והחלפה (שיכולה להיות מחרוזת ריקה כדי למחוק).

## ראו גם
- [Clojure רשמי למידע על מחרוזות](https://clojure.org/guides/learn/functions#_strings)
- [מדריך לביטויים רגולריים ב-Clojure](https://clojure.org/guides/learn/syntax#_regular_expressions)
- [clojure.string API על clojuredocs](https://clojuredocs.org/clojure.string)
