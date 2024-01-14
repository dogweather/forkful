---
title:                "Clojure: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

# למה?
שליחת בקשת HTTP היא דרך נפלאה לתקשורת בין קוד לשרת ולפנות למידע חיצוני או לשמירה של מידע בשרת. זהו כלי חשוב בעולם התכנות שיכול להקל על עבודתכם ולהגביר את היצירתיות שלכם.

# איך לעשות זאת?
תחילה עליכם להתחבר לכתובת URL של השרת לו אתם רוצים לשלוח בקשה. לדוגמה:

```Clojure
(def url "https://www.example.com/api/users")
```

לאחר מכן, עליכם לבחור את סוג הבקשה הנכון על ידי שימוש בפונקציית `clj-http.client/request`. לדוגמה, אם ברצונכם לשלוח בקשה GET לכתובת URL שלנו, צרו את הבקשה הבאה:

```Clojure
(def req {:method :get :url url})
```

לאחר מכן, עליכם לשלוח את הבקשה על ידי התחברות לשרת:

```Clojure
(def res (clj-http.client/request req))
```

כעת, תוכלו לראות את התגובה מהשרת על ידי הדפסת האובייקט `res`:

```Clojure
(prn res)
```

## Deep Dive
כדי לשלוח בקשות מתקדמות יותר, ניתן להשתמש במגוון פונקציות ואפשרויות של `clj-http` כדי להתאים את הבקשה לצרכים המיוחדים שלכם. לדוגמה, ניתן להגדיר כותרת (`headers`) או גוף (`body`) לבקשה באמצעות מילון שמיר כארגומנטים לפונקציית `request`. ניתן גם להגדיר את סוג המידע בתשובה כדי להקל על עיבוד המידע.

# ראה גם
- [מדריך ל־clj-http](https://github.com/dakrone/clj-http)
- [מדריך לשליחת בקשות HTTP ב־Clojure](https://luminusweb.com/docs/http-client.md)