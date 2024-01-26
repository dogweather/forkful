---
title:                "מחיקת תווים התואמים לתבנית"
date:                  2024-01-20T17:42:06.357258-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים שתואמים דפוס היא לקחת טקסט ולשלול ממנו כל חלק שמתאים למודל מסוים. תכנתנים עושים את זה כאשר הם רוצים לנקות נתונים או להפוך אותם לאחידים יותר.

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
