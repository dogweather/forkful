---
date: 2024-01-26 00:51:59.305858-07:00
description: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05D6\u05D4 \u05DC\u05E0\u05D4\u05DC \u05D0\u05EA \u05D4\u05D1\u05DC\u05EA\u05D9\
  \ \u05E6\u05E4\u05D5\u05D9 \u05D1\u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA\u2014\
  \u05DB\u05DE\u05D5 \u05D0\u05D1\u05D8\u05D7 \u05D1\u05DE\u05D5\u05E2\u05D3\u05D5\
  \u05DF \u05E9\u05DE\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD \u05DE\u05D7\u05D5\
  \u05DC\u05DC\u05D9 \u05D1\u05E2\u05D9\u05D5\u05EA. \u05DC\u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05D0\u05D4\u05D1\u05D4 \u05DC\u05D7\u05DC\u05E7; \u05D8\u05D9\
  \u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05E2\u05D5\u05D6\
  \u05E8 \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E2\u05DC \u05D4\u05D1\u05E2\u05D9\u05D5\
  \u05EA \u05D1\u05E7\u05D5, \u05DE\u05D1\u05D8\u05D9\u05D7\u2026"
lastmod: '2024-03-13T22:44:38.716849-06:00'
model: gpt-4-1106-preview
summary: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05D6\u05D4 \u05DC\u05E0\u05D4\u05DC \u05D0\u05EA \u05D4\u05D1\u05DC\u05EA\u05D9\
  \ \u05E6\u05E4\u05D5\u05D9 \u05D1\u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA\u2014\
  \u05DB\u05DE\u05D5 \u05D0\u05D1\u05D8\u05D7 \u05D1\u05DE\u05D5\u05E2\u05D3\u05D5\
  \u05DF \u05E9\u05DE\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD \u05DE\u05D7\u05D5\
  \u05DC\u05DC\u05D9 \u05D1\u05E2\u05D9\u05D5\u05EA."
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
weight: 16
---

## מה ולמה?
טיפול בשגיאות זה לנהל את הבלתי צפוי בתוכניות—כמו אבטח במועדון שמתמודד עם מחוללי בעיות. למתכנתים אהבה לחלק; טיפול בשגיאות עוזר לשמור על הבעיות בקו, מבטיח שהקוד שלהם לא יתנפל ויפול כשהם מתמודדים עם הבלתי צפוי.

## איך לעשות:
Clojure, כמו אבותיו ה-Lisp, נשען על חריגים למטרת טיפול בשגיאות. הנה איך אתה מראה מה יש לך כשהדברים הולכים לאיבוד.

זריקת חריג היא ישירה:
```Clojure
(throw (Exception. "אופס! משהו השתבש."))
```

תפיסת חריג, תעשה את זה הרבה:
```Clojure
(try
  ;; קוד מסוכן
  (/ 1 0)
  (catch ArithmeticException e
    (println "לא ניתן לחלק באפס!"))
  ;; finally block ירוץ לא משנה מה
  (finally 
    (println "קוד לניקוי מופיע כאן.")))
```
פלט לדוגמא עבור הבלוק catch לעיל:
```
לא ניתן לחלק באפס!
קוד לניקוי מופיע כאן.
```

שימוש ב-`ex-info` ו-`ex-data` עבור הקשר עשיר יותר לגבי חריגים:
```Clojure
(try
  ;; גרימה לחריג מותאם אישית
  (throw (ex-info "שגיאה מותאמת אישית" {:type :custom-failure}))
  (catch Exception e
    ;; חילוץ הנתונים מהחריג המותאם אישית שלנו
    (println (ex-data e))))
```
פלט לדוגמא:
```
{:type :custom-failure}
```

## צלילה עמוקה
סיפור טיפול השגיאות ב-Clojure אינו שונה מהותית משפות ליספ אחרות או אפילו מ-Java (ממנו הוא יורש את מנגנון ה-`try-catch`). זה פרגמטי; שימוש בחריגים הוא הדרך הראשית, בדיוק כמו ב-Java, אך Clojure מציע טעם פונקציונאלי עם `ex-info` ו-`ex-data` לנתוני שגיאה עשירים יותר.

אלטרנטיבות לטיפול בשגיאות ב-Clojure כוללות שימוש במבנים מונדיים, כמו מונד `either` מספריות כמו `cats`, או core.async להפצת שגיאות מבוססת ערוצים. עם זאת, אלה מורכבות יותר ומשמשות בתרחישים ספציפיים.

באופן היסטורי, טיפול בשגיאות בשפות תכנות התפתח מחזרה פשוטה של סטטוסים למנגנוני טיפול בחריגים יותר מתוחכמים של שפות מודרניות. Clojure בוחר בפשטות ובנגיעה של תכנות פונקציונלי, משלב בין הישן והחדש.

## ראה גם
- מדריך Clojure לחריגים: https://clojure.org/guides/exceptions
- ספריית "Cats" לגישות יותר פונקציונאליות: https://github.com/funcool/cats
- "Core.async" לתכנות אסינכרוני: https://github.com/clojure/core.async
