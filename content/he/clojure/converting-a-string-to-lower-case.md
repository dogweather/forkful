---
title:                "להמיר מחרוזת לאותיות קטנות"
html_title:           "Clojure: להמיר מחרוזת לאותיות קטנות"
simple_title:         "להמיר מחרוזת לאותיות קטנות"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מדוע
מתכנתים משתמשים בפונקציות להמרת מחרוזות לאותיות קטנות, היתרון העיקרי של כך הוא לסדר ולייעל את הקוד שלהם. כאשר משתמשים באותיות קטנות, ניתן לכתוב את הקוד בצורה יותר נקייה וקריאה יותר, וכן למנוע שגיאות בעת השוואה בין מחרוזות.

## איך להמיר מחרוזת לאותיות קטנות
```Clojure
(clojure.string/lower-case "WELCOME TO CLOJURE") ;; "welcome to clojure"

(clojure.string/lower-case "הגיע הזמן להתחיל עם קלוז'ור") ;; "הגיע הזמן להתחיל עם קלוז'ור"
```

## כיוון עמוק
הודות לפונקציות כמו lower-case, ניתן לייעל קוד ולשפר את הקריאות שלו. כאשר משתמשים בפונקציה זו, חשוב לציין שמחרוזת תועבר שהערך שלה הוא לא משתנה, כלומר שהיא לא תשנה את הערך של המחרוזת המקורית, אלא שתחזיר ערך חדש. משתמשים ב-(string/lower-case) של clojure ניתן להנות מכל היתרונות של תכנות בקלות ובחזרה על עצמך, אסור לשנות מוח את הערך המקורי של המחרוזת.

## ראו גם
- פונקציית lower-case ב-Clojure: https://clojuredocs.org/clojure.string/lower-case
- הוראות כתיבה נקודותיות וקצרות בלינג אלג'יר של סוג של כתיבות לא טבעיות: https://clojure.org/guides/property_calling_syntax_he
- עיצוב טיולים בסיוט ב -Clojure: https://www.clojurist.com/2016/is-mysql-still-relevant/
- Clojure פונקציות כאלה להתייחס לתכנית שבוע בלוג: https://www.n surprises-תכנות/internet
- תיעוד זה מאפשר גם מידע על הפונקציות הרבות המיוחדות בתחום התכנות בקלוארי: http://map.jalada.co.za/