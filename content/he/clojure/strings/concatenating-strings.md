---
date: 2024-01-20 17:34:56.643868-07:00
description: "\u05E6\u05D9\u05E8\u05D5\u05E3 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D4\u05D5\u05D0 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\u05E8\u05D5\u05DB\u05D4 \u05D9\u05D5\u05EA\
  \u05E8 \u05DE\u05E9\u05EA\u05D9\u05D9\u05DD \u05D0\u05D5 \u05D9\u05D5\u05EA\u05E8\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D9\
  \u05D5\u05EA\u05E8. \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05D4\u05E8\u05DB\u05D9\u05D1 \u05DE\u05D9\
  \u05D3\u05E2 \u05DC\u05D4\u05E6\u05D2\u05D4, \u05DC\u05D9\u05E6\u05D9\u05E8\u05EA\
  \ \u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05DD \u05D3\u05D9\u05E0\u05D0\u05DE\u05D9\
  \u05D9\u05DD, \u05D0\u05D5 \u05DC\u05E9\u05D9\u05DC\u05D5\u05D1\u2026"
lastmod: '2024-03-13T22:44:38.690982-06:00'
model: gpt-4-1106-preview
summary: "\u05E6\u05D9\u05E8\u05D5\u05E3 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D4\u05D5\u05D0 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\u05E8\u05D5\u05DB\u05D4 \u05D9\u05D5\u05EA\
  \u05E8 \u05DE\u05E9\u05EA\u05D9\u05D9\u05DD \u05D0\u05D5 \u05D9\u05D5\u05EA\u05E8\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D9\
  \u05D5\u05EA\u05E8. \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05D4\u05E8\u05DB\u05D9\u05D1 \u05DE\u05D9\
  \u05D3\u05E2 \u05DC\u05D4\u05E6\u05D2\u05D4, \u05DC\u05D9\u05E6\u05D9\u05E8\u05EA\
  \ \u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05DD \u05D3\u05D9\u05E0\u05D0\u05DE\u05D9\
  \u05D9\u05DD, \u05D0\u05D5 \u05DC\u05E9\u05D9\u05DC\u05D5\u05D1\u2026"
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
צירוף מחרוזות הוא פשוט ליצור מחרוזת ארוכה יותר משתיים או יותר מחרוזות קטנות יותר. תכנותים עושים זאת להרכיב מידע להצגה, ליצירת פורמטים דינאמיים, או לשילוב נתונים.

## איך לעשות:
```Clojure
; בסיס ודוגמאות

(str "שלום " "עולם") ; => "שלום עולם"
(apply str ["בוקר " "טוב"]) ; => "בוקר טוב"

; שילוב של משתנים ומחרוזות
(def שם "דוגמה")
(str "זו היא " שם " פשוטה") ; => "זו היא דוגמה פשוטה"
```

## טבילה עמוקה
בקהילת הפיתוח המודרנית, צירוף מחרוזות הוא כלי יום-יומי. מחרוזת היא סדרה אינהומוגנית של תווים, ב-Clojure כמו בשפות LISP אחרות, מחרוזת מיוצגת כציטוט כפול. בעבר, גישות שונות לצירוף מחרוזות כללו אריתמטיקה של מצביעים ועבודה עם שפת המכונה ישירות. עם זאת, בשפות המודרניות, פונקציות כמו `str` ב-Clojure מפשטות את התהליך.

לאלטרנטיבות נכללות פונקציות כמו `format` שמאפשר הוספת פרמטרים לתבנית, ו-`StringBuilder` ב-Java שמגיע דרך Java Interop ב-Clojure לשילובים אופטימליים של מחרוזות במקרים שבהם יש צורך ליצור מחרוזות ענקיות.

לגבי פרטי המימוש, ככול שהתארכות המחרוזת גדלה, חשוב להיות מודע לכך שייתכן שזה יהיה פחות יעיל מבחינת זמן-ריצה ושימוש בזיכרון. אופטימיזציה של צירופי מחרוזות יכולה לכלול שימוש במבנים נתון מתאימים, כמו וקטורים או רשימות שהשימוש בהם נעשה עם יותר זהירות.

## גם כדאי לראות
- [ClojureDocs ל `str`](https://clojuredocs.org/clojure.core/str)
- [מדריך Clojure](https://clojure.org/guides/learn/syntax)
- [דיון ב-StackOverflow על צירוף מחרוזות ב-Clojure](https://stackoverflow.com/questions/1912343/how-to-concatenate-strings-in-clojure)
- [תיעוד Java Interop ב-Clojure](https://clojure.org/reference/java_interop)
