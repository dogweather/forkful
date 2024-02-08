---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
aliases:
- he/clojure/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:12.714299-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

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
