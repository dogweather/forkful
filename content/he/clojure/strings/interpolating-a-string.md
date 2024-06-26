---
date: 2024-01-20 17:51:41.403012-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05E7\u05DC\
  \u05D5\u05D6'\u05E8 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D1\u05E6\u05D5\u05E8\u05D4 \u05E9\u05D5\u05E0\u05D4 \u05DE\u05E9\
  \u05E4\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA, \u05DB\u05D9 \u05D0\u05D9\u05DF\
  \ \u05DC\u05E0\u05D5 \u05D0\u05EA \u05D4\u05DE\u05EA\u05D5\u05D3\u05D4 'interpolate'\
  \ \u05DB\u05DE\u05D5 \u05D1-Ruby \u05D0\u05D5 \u05D1-Python. \u05D0\u05D1\u05DC\
  \ \u05D0\u05DC \u05D3\u05D0\u05D2\u05D4, \u05D9\u05E9 \u05DC\u05E0\u05D5\u2026"
lastmod: '2024-03-13T22:44:38.681602-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E7\u05DC\u05D5\u05D6'\u05E8 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1\u05E6\u05D5\u05E8\u05D4 \u05E9\u05D5\
  \u05E0\u05D4 \u05DE\u05E9\u05E4\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA, \u05DB\
  \u05D9 \u05D0\u05D9\u05DF \u05DC\u05E0\u05D5 \u05D0\u05EA \u05D4\u05DE\u05EA\u05D5\
  \u05D3\u05D4 'interpolate' \u05DB\u05DE\u05D5 \u05D1-Ruby \u05D0\u05D5 \u05D1-Python."
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 8
---

## איך לעשות:
בקלוז'ר עושים אינטרפולציה של מחרוזות בצורה שונה משפות אחרות, כי אין לנו את המתודה 'interpolate' כמו ב-Ruby או ב-Python. אבל אל דאגה, יש לנו מקרופים ופונקציות שיכולות לעשות את העבודה.
```clojure
;; השימוש ב-str לאיחוד מחרוזות וערכים
(def name "אברהם")
(str "שלום, " name ", מה שלומך?")

;; פלט: "שלום, אברהם, מה שלומך?"

;; השימוש ב-formatted לפורמט מחרוזת עם ערכים מפורמטים
(defn greet [name]
  (format "שלום %s, על איזה מספר אתה חושב?" name))

(greet "יעקב")
;; פלט: "שלום יעקב, על איזה מספר אתה חושב?"
```

## צלילה לעומק
בקלוז'ר, אינטרפולציה של מחרוזות אינה כוללת אופציה בתוך השפה כמו שפות אחרות. היא באה ממסורת של שפות תכנות פונקציונליות כמו ליספ, שבהן היה יותר נפוץ לבנות מחרוזות מרכיבים קטנים יותר, מאשר לכתוב רצפי מחרוזות ארוכים עם ערכים מוטמעים בתוכם.

גישה אחרת שניתן להשתמש בה היא השימוש בתקני עיצוב של Java באמצעות הפונקציה `format`. חשוב לזכור שכל שימוש בפונקציה זו יהיה פחות "Clojurian" כי היא מתרחקת מהפשטות והיעילות של רכיבה מרכיבים פשוטים.

רכיבים כמו המקרו `str` מספקים דרך יעילה ופשוטה ליצירת מחרוזות דינמיות. כשצריך להוסיף ערכים משתנים לתוך מחרוזות, פעמים רבות מומלץ לשקול את `str`, `format`, או אפילו פיצ'רים נוספים כמו טמפלייטים או עיצוב עם דאטה-ריידרס כדי לנהל קטעי טקסט גדולים יותר ומורכבים.

## ראה גם
- [Clojure תיעוד של `str`](https://clojuredocs.org/clojure.core/str)
- [Clojure תיעוד של `format`](https://clojuredocs.org/clojure.core/format)
- [Clojure פורום לדיונים בנושאי תכנות](https://ask.clojure.org/)
