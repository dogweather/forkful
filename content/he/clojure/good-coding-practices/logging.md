---
date: 2024-01-26 01:03:24.966758-07:00
description: "\u05DC\u05D5\u05D2\u05D9\u05E0\u05D2 \u05D4\u05D5\u05D0 \u05D1\u05E2\
  \u05E6\u05DD \u05D4\u05DE\u05E7\u05D1\u05D9\u05DC \u05D4\u05EA\u05D5\u05DB\u05E0\
  \u05EA\u05D9 \u05E9\u05DC \u05D9\u05D5\u05DE\u05DF \u05E1\u05E4\u05D9\u05E0\u05D4\
  ; \u05D6\u05D4 \u05D3\u05E8\u05DA \u05DC\u05EA\u05E2\u05D3 \u05D0\u05D9\u05E8\u05D5\
  \u05E2\u05D9\u05DD \u05E9\u05E7\u05D5\u05E8\u05D9\u05DD \u05D1\u05D6\u05DE\u05DF\
  \ \u05E9\u05D9\u05D9\u05E9\u05D5\u05DD \u05E8\u05E5. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8 \u05D0\u05D9\u05E8\u05D5\u05E2\
  \u05D9\u05DD \u05D0\u05DC\u05D5 \u05DC\u05E6\u05D5\u05E8\u05DA \u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD,\u2026"
lastmod: '2024-02-25T18:49:37.027699-07:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D5\u05D2\u05D9\u05E0\u05D2 \u05D4\u05D5\u05D0 \u05D1\u05E2\u05E6\
  \u05DD \u05D4\u05DE\u05E7\u05D1\u05D9\u05DC \u05D4\u05EA\u05D5\u05DB\u05E0\u05EA\
  \u05D9 \u05E9\u05DC \u05D9\u05D5\u05DE\u05DF \u05E1\u05E4\u05D9\u05E0\u05D4; \u05D6\
  \u05D4 \u05D3\u05E8\u05DA \u05DC\u05EA\u05E2\u05D3 \u05D0\u05D9\u05E8\u05D5\u05E2\
  \u05D9\u05DD \u05E9\u05E7\u05D5\u05E8\u05D9\u05DD \u05D1\u05D6\u05DE\u05DF \u05E9\
  \u05D9\u05D9\u05E9\u05D5\u05DD \u05E8\u05E5. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8 \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\
  \u05DD \u05D0\u05DC\u05D5 \u05DC\u05E6\u05D5\u05E8\u05DA \u05E0\u05D9\u05E4\u05D5\
  \u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD,\u2026"
title: "\u05DC\u05D5\u05D2\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
לוגינג הוא בעצם המקביל התוכנתי של יומן ספינה; זה דרך לתעד אירועים שקורים בזמן שיישום רץ. מתכנתים עושים זאת כדי לעקוב אחר אירועים אלו לצורך ניפוי באגים, מעקבים פיננסיים, או כדי להשיג הבנה על התנהגות של מערכת בסביבת ייצור.

## איך לעשות:
Clojure מתבססת על אמצעי הלוגינג של Java, אך ניתן להשתמש בהם בדרך אידיומטית יותר של Clojure. בואו נביט באיך אתה יכול להשתמש ב- `clojure.tools.logging`, שמספק אבסטרקציה פשוטה מעל מספר פרייםוורקים ללוגינג:

ראשית, הוסף תלות ל- `clojure.tools.logging` ויישום לוגינג כמו `log4j` בקובץ ה- `project.clj` שלך:

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

עכשיו, בואו נתעד כמה הודעות:

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "מתחיל חישוב מאומץ...")
  (Thread/sleep 3000) ; סימולציה של חישוב ארוך
  (log/info "חישוב הסתיים. התשובה היא 42.")
  42)

(compute-answer-to-everything)
```
הפלט לא יראה הודעות `DEBUG` כברירת מחדל, מכיוון שרמות הלוג נקבעות בדרך כלל ל- `INFO`:

```
INFO  [your-namespace] - חישוב הסתיים. התשובה היא 42.
```

ניתן להגדיר את רמות הלוג והמעבדים בקובץ `log4j.properties` כדי לקבל פלט מרובה יותר אם נחוץ.

## לעומק
`clojure.tools.logging` של Clojure קיימת כבר תקופה ומשמשת כגשר בין קוד Clojure לעולם הלוגינג של Java. בהיסטוריה, Java עברה כמה איטרציות וספריות שונות ללוגינג כמו את API הלוגינג הכלול ב-Java, `log4j`, `slf4j`, ו-`logback`.

ב-Clojure, בעוד שאתם יכולים להשתמש ישירות בפרייםוורקים ללוגינג של Java, `clojure.tools.logging` מזהה ומפנה לכל פרייםוורק לוגינג שהוא מוצא ב-classpath שלכם, וכך חוסך מכם להיות קשורים באופן קשה ליישום ספציפי. זה יכול לעזור לשמור את קוד ה-Clojure שלכם נייד ומודולרי יותר.

אלטרנטיבות ל- `clojure.tools.logging` באקוסיסטם של Clojure כוללות ספריות כמו `timbre`, שהיא ספריית לוגינג טהורה של Clojure עם מאפיינים כמו סיבוב לוגים, סינון ולוגינג אסינכרוני מובנה.

פרטי היישום הם קריטיים כאשר מדובר בלוגינג בסביבה רב-תהליכית כמו Clojure. כאן, אי-השינויות וניהול השפעות הצדדיות מספקים יתרונות ברורים. לוגינג, כשפעת צד, צריך להיטפל בזהירות כדי למנוע מכשולי ביצועים ולוודא תחסום לפי קוואים(thread-safety), שרוב פרייםוורקים ללוגינג של Java כבר דואגים לזה.

לבסוף, שקלו לוגינג מובנה, שבו הלוגים כתובים כנתונים מובנים (כמו JSON). זה יכול להיות שימושי ביותר לניתוח ועיבוד מאוחר יותר, במיוחד כאשר מתמודדים עם מערכות מבוזרות בקנה מידה גדול.

## ראה גם
אם אתם רוצים עוד, שקלו לבדוק את משאבים אלו:

- תיעוד Clojure Tools Logging: https://github.com/clojure/tools.logging
- Timbre, ספריית לוגינג של Clojure: https://github.com/ptaoussanis/timbre
- קונפיגורציה של Log4J ב-Clojure: http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- ספר ההוראות של Logback להגדרות מתקדמות: http://logback.qos.ch/manual/
- מדריך ללוגינג מובנה ב-Clojure: https://corfield.org/blog/2020/04/28/structured-logging/
