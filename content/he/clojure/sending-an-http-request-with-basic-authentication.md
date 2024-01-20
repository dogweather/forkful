---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי, היא פתרון נוח, מהיר ופשוט לאימות משתמשים. מתכנתים משתמשים בזה כדי להבטיח שרק משתמשים מאומתים יהיו מסוגלים לגשת למשאבים מסוימים באפליקציה.

## איך לעשות: 
בואו נניח שאנחנו שולחים בקשת GET ל 'https://example.com' עם שם משתמש וסיסמה באימות. מדובר באופן מקובל לביצוע אימות בסיסי:

```Clojure
(require '[clj-http.client :as client])

(let [auth (client/basic-auth "username" "password")]
  (client/get "https://example.com" {:basic-auth auth}))
```

אנחנו מגדירים שם משתמש וסיסמה ואז שולחים את הבקשה עם שימוש בפונקציה `client/get`.

## צלילה עמוקה:
שליחת בקשות HTTP עם אימות בסיסי היא שיטה מגוחכת בפרוטוקול HTTP מאז 1996 (RFC 1945). קיימות חלופות אחרות קיימות לאימות, כולל oAuth וJWT, אך אימות בסיסי משמש עדיין למשימות פשוטות כי הוא פשוט להגדיר ולהשתמש.

חשוב לציין, שאימות בסיסי שולח את שם המשתמש והסיסמה בפורמט "Base64" בפליינטקסט, מה שאומר שהוא לא מספק שום אמת מחצבת ברמת האבטחה. שימוש באימות בסיסי ממולץ לאפליקציות שמשתמשות ב HTTPS.

## קישורים נוספים: 
- [ספר ייעודי של Clojure לשליחת בקשות HTTP](https://github.com/dakrone/clj-http)
- [אימות בסיסי בוויקיפדיה](https://he.wikipedia.org/wiki/%D7%90%D7%99%D7%9E%D7%95%D7%AA_%D7%91%D7%A1%D7%99%D7%A1%D7%99)
- [RFC 1945 - פרוטוקול העברת טקסט היפר](https://tools.ietf.org/html/rfc1945)