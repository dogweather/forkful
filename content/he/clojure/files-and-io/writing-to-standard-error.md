---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:14.872262-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4\
  \ \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05E2\u05D5\u05E1\u05E7\
  \u05EA \u05D1\u05D4\u05E4\u05E0\u05D9\u05D9\u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\
  \u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D5\u05D0\u05D1\u05D7\u05D5\u05E0\u05D9\
  \u05DD \u05DC\u05D6\u05E8\u05DD stderr, \u05D1\u05E0\u05E4\u05E8\u05D3 \u05DE\u05E4\
  \u05DC\u05D8 \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9 (stdout). \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05D4\u05D1\u05D3\u05D9\u05DC \u05D1\u05D9\u05DF \u05E4\u05DC\
  \u05D8 \u05EA\u05DB\u05E0\u05D9\u05EA\u2026"
lastmod: 2024-02-19 22:04:58.004052
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05E1\
  \u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05E2\u05D5\u05E1\u05E7\u05EA\
  \ \u05D1\u05D4\u05E4\u05E0\u05D9\u05D9\u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA\
  \ \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D5\u05D0\u05D1\u05D7\u05D5\u05E0\u05D9\u05DD\
  \ \u05DC\u05D6\u05E8\u05DD stderr, \u05D1\u05E0\u05E4\u05E8\u05D3 \u05DE\u05E4\u05DC\
  \u05D8 \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9 (stdout). \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\
  \u05D9 \u05DC\u05D4\u05D1\u05D3\u05D9\u05DC \u05D1\u05D9\u05DF \u05E4\u05DC\u05D8\
  \ \u05EA\u05DB\u05E0\u05D9\u05EA\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לשגיאה סטנדרטית (stderr) עוסקת בהפניית הודעות שגיאה ואבחונים לזרם stderr, בנפרד מפלט סטנדרטי (stdout). מתכנתים עושים זאת כדי להבדיל בין פלט תכנית רגיל לבין הודעות שגיאה, מה שמאפשר איתור באגים ורישום יומנים ביעילות רבה יותר.

## איך לעשות:
בClojure, ניתן לכתוב ל stderr באמצעות הזרם `*err*`. הנה דוגמה בסיסית:

```clojure
(.write *err* "This is an error message.\n")
```

שימו לב שלאחר כתיבת הודעה, עליכם לרוקן את הזרם כדי להבטיח שההודעה תוצא מיד:

```clojure
(flush)
```

דוגמת פלט ל stderr:
```
This is an error message.
```

אם אתם מתמודדים עם חריגות, יכול להיות שתרצו להדפיס מעקבי ערימה ל stderr. השתמשו ב`printStackTrace` בשביל זה:

```clojure
(try
  ;; קוד שעלול לזרוק חריגה
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

לרישום שגיאות מובנה יותר, ניתן להגדיר ספריות צד שלישי כמו `timbre` לרישום ל stderr. הנה הגדרה ושימוש בסיסיים:

ראשית, הוסיפו את `timbre` לתלותיות שלכם. לאחר מכן הגדירו אותו לשימוש ב stderr:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; ביטול רישום בstdout
(timbre/set-config! [:appenders :spit :enabled?] false) ;; ביטול רישום בקובץ
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; הפעלת stderr לשגיאות

(timbre/error "An error occurred while processing your request.")
```

זה יוביל הודעות ברמת שגיאה ל stderr, ויהפוך אותן לברורות מפלט היישום הסטנדרטי.
