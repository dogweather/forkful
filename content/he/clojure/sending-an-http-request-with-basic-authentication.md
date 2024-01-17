---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Clojure: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא תהליך בו מתכנתים משתמשים בכדי לאמת ולאשר את הגישה לשירות או למשאב מסוים באמצעות פרטי הכניסה הבסיסיים (שם משתמש וסיסמה). כך, ניתן להבטיח שרק משתמשים מורשים יוכלו לגשת למידע או לבצע פעולות מסוימות. 

למה מתכנתים עושים את זה? בדרך כלל, זה משמש כחלק מתהליך אבטחת המידע ואימות משתמשים במערכת. כמו כן, זה מאפשר למתכנתים לשלוט ברמת הגישה לשירותים שונים ולהגביל אותם כרצונם.

# איך לעשות?
```Clojure
(require '[clj-http.client :as client]) 

(def response (client/get "https://example.com/api" 
            :basic-auth ["username" "password"])) 

(println (:status response))
(println (:body response))
```

כאן, אנו משתמשים בספריית clj-http כדי לשלוח בקשת GET לכתובת ה-URL של ה-API של אתר נתונים. בכדי לאמת את הגישה, אנו מעבירים את שם המשתמש והסיסמה שלנו כחלק מפרמטר ה-basic-auth בבקשה. הודות לכך, אנו מקבלים תשובה שלאתר נתונים מאשרת ומאפשרת לנו לגשת למידע.

# הצצה עמוקה
השימוש באימות בסיסי לשליחת בקשות HTTP הוא נפוץ מאוד ביישומים שונים כיום. ניתן להשתמש גם באימות עם OAuth או אבטחת JWT כדי לשנות את המתאם לצורך בקשת HTTP עם אימות. בנוסף, ישנם ספריות נוספות שניתן להשתמש בהן כדי לשלוח בקשות עם אימות בסיסי, כגון http-kit ו-ring.

# ראו גם
לפרטים נוספים על שימוש באימות בסיסי לשליחת בקשות HTTP, ניתן לעיין במדריכים ובמסמכים הבאים:

- [מדריך על אימות בסיסי עם clojure-http](https://github.com/dakrone/clj-http/wiki/Basic-Authentication)
- [מסמך על HTTP ואבטחת המערכת](https://lambdaisland.com/blog/2016-10-20-a-lambda-a-day-http-and-security)