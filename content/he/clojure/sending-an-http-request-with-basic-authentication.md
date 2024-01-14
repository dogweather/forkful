---
title:                "Clojure: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## כיצד לשלוח בקשת HTTP עם אימות בסיסי ב-Clojure

כדי לשלוח בקשת HTTP עם אימות בסיסי ב-Clojure יש למלא את הפרמטרים הבאים:
- URL: כתובת האתר שברצונך לשלוח אליו את הבקשה
- Username: שם משתמש שיכול לגשת לאתר זה
- Password: סיסמא עבור המשתמש הנ"ל

עכשיו, יש לבצע אימות בסיסי באמצעות הפרוטוקול הנכון ולשלוח את הבקשה.

## דוגמאות קוד ופלט

```Clojure
;; ייבא חפיסת התאמות HTTP
(require '[clj-http.client :as http])

;; הגדרת פרמטרי הבקשה
(def url "https://example.com/api/users")
(def username "myusername")
(def password "mypassword")

;; ביצוע בקשה עם אימות בסיסי ותצוגת תוצאות הפלט
(http/get url
        {:basic-auth [username password]})
=> {:status 200, :headers {"Content-Type" "application/json"}, :body "[{\"id\": 1, \"name\": \"John\"}, {\"id\": 2, \"name\": \"Jane\"}]"}
```

## שילוב עמוק

כאשר אנו משתמשים באימות בסיסי, אנו מאפשרים למשתמש לגשת אל משאבי האתר באופן מאובטח. כיצד זה עובד? הבקשת HTTP עם אימות בסיסי כוללת השלמת כותרת המתאימה "Basic", כאשר מאחר והכלים הם בעלי תוספת בסיסית, הם מאפשרים לגשת אל משאבי האתר באופן מאובטח.

כאשר אנו משתמשים ב-Clojure לשלוח בקשה עם אימות בסיסי, אנו משתמשים בפונקציית `get` מחבילת התאמות ה-HTTP לבצע את הפעולות הנדרשות. לאחר מכן, אנו עוברים על הפרמטרים הנחוצים כדי לקבל את כתובת האתר, שם המשתמש והסיסמא ומשתמשים באופן תקין בפונקציה `basic-auth` לבצע את האימות הבסיסי.

עם זאת, לכידת השגיאות הנדרשות ב-Clojure תלויה גם בתפקיד הטכנול