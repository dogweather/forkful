---
date: 2024-01-20 18:01:12.714299-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05DE\u05E6\u05D9\u05D2\u05D9\
  \u05DD \u05D6\u05D4\u05D5\u05EA (\u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D5\
  \u05E1\u05D9\u05E1\u05DE\u05D4) \u05D1\u05DB\u05D5\u05EA\u05E8\u05EA \u05E9\u05DC\
  \ \u05D4\u05D1\u05E7\u05E9\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D2\
  \u05D9\u05E9\u05D4 \u05DC\u05DE\u05E9\u05D0\u05D1 \u05DE\u05D5\u05D2\u05DF. \u05EA\
  \u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E2\u05DC\
  \u2026"
lastmod: '2024-03-13T22:44:38.703781-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05DE\u05E6\u05D9\u05D2\u05D9\
  \u05DD \u05D6\u05D4\u05D5\u05EA (\u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D5\
  \u05E1\u05D9\u05E1\u05DE\u05D4) \u05D1\u05DB\u05D5\u05EA\u05E8\u05EA \u05E9\u05DC\
  \ \u05D4\u05D1\u05E7\u05E9\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D2\
  \u05D9\u05E9\u05D4 \u05DC\u05DE\u05E9\u05D0\u05D1 \u05DE\u05D5\u05D2\u05DF. \u05EA\
  \u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E2\u05DC\
  \u2026"
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
weight: 45
---

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא תהליך שבו מציגים זהות (שם משתמש וסיסמה) בכותרת של הבקשה כדי לקבל גישה למשאב מוגן. תכניתנים עושים את זה כדי לשמור על ביטחון המידע ולאמת גישת משתמשים ל-APIs ולמשאבים ברשת.

## איך לעשות:
דוגמה לקוד שולח בקשת GET עם אימות בסיסי ב-Clojure:

```Clojure
(require '[clj-http.client :as client])

(defn send-http-request-with-basic-auth
  [url username password]
  (let [credentials (str (b64/encode (str username ":" password)))]
    (:status (client/get url {:headers {"Authorization" (str "Basic " credentials)}}))))

(println (send-http-request-with-basic-auth "http://example.com" "myuser" "mypass"))
```

פלט לדוגמה:

```
200
```

שימו לב: תתכן צורך לעטוף את התוצאה ב`println` כדי לזהות את קוד המענה בבירור.

## עומק המידע:
האימות הבסיסי ב-HTTP נעשה באמצעות קידוד בבסיס 64 של שם המשתמש והסיסמה, מופרדים בנקודתיים. הוא נחשב לפשוט אך לא הכי בטוח, מכיוון שלא מציע הצפנה של מידע האימות. אלטרנטיבות נפוצות כוללות אימות דיגיטלי, OAuth או JWT (JSON Web Tokens), אשר מציעים שיפור באבטחה. לעומת זאת, אימות בסיסי עדיין נמצא בשימוש נרחב במקרים בהם פשטות הוא גורם חשוב, ובתשתיות עם וקטורי איום מוגבלים.

## ראה גם:
- [clj-http documentation](https://github.com/dakrone/clj-http)
- [RFC 7617, The 'Basic' HTTP Authentication Scheme](https://datatracker.ietf.org/doc/html/rfc7617)
- [Understanding Base64 Encoding](https://en.wikipedia.org/wiki/Base64)
