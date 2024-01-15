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

שלום לקוראיי הישראלים! אני מתחם קלוז'ר ואני אשתף אתכם כמה דברים על שליחת בקשת HTTP עם אימות בסיסי (basic authentication). אם אתם משתמשים בפתרונות בענן או בכלים דינמיים כמו דוקר, אתם כנראה תצטרכו לפעמים לשלוח בקשת HTTP עם אימות בסיסי. אני אראה לכם את הדרך הקלה והפשוטה ביותר לעשות זאת בקלוז'ר.

## Why

המערכת המודרנית של האינטרנט משתמשת בתקשורת HTTP בכדי להתחבר בין שרתים ומכשירים. כשאנחנו משתמשים באתרים או ביישומים, אנחנו משלחים בקשות HTTP כדי לקבל תגובות מהשרתים המארחים אותם. בכמה מקרים, אנחנו צריכים ליצור חיבור מאובטח עם אתר מסוים בכדי לגשת לפרטי חשבון או למידע רגיש. בכדי לעשות זאת, אנו נשלח בקשת HTTP עם אימות בסיסי, כדי להתאמן כמו שאנחנו עושים זאת בכל טכנולוגיה אינטרנטית אחרת.

## How To

בקשת HTTP עם אימות בסיסי מתבצעת בשני שלבים חשובים: הכנת הכותרת (header) והכנת הגוף (body). ראשית, בכתובת ה-URL של הבקשה אנו מוסיפים את שם המשתמש והסיסמה מופרדים בדליל נקודות: 

```Clojure
"http://username:password@www.example.com"
```

הכותרת משמשת כדי לספק את מידע האימות הכרוך בבקשה. קוד הקלוז'ר הרשום מטה מתאר איך להוסיף את הכותרת באופן אוטומטי בעזרת ספריית clj-http:

```Clojure
(require '[clj-http.client :as client])

(def response
  (client/get "http://www.example.com"
              {:basic-auth ["username" "password"]}))
```

בגוף הבקשה, אנחנו מעבירים את הפרמטרים הנדרשים