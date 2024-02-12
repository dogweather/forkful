---
title:                "כתיבה לשגיאה התקנית"
aliases:
- he/clojure/writing-to-standard-error.md
date:                  2024-02-03T19:33:14.872262-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
