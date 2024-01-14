---
title:                "Clojure: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה
אנשים מופנים להוריד את דף האינטרנט רק במקרים מסוימים, לדוגמה כאשר הם מעוניינים לשמור את התוכן עבור צפייה במקום בלתי מקוון או לשימוש במטרות מחקר.

## איך לעשות זאת
תחתית: כדי להוריד דף אינטרנט ב-Clojure, ניתן להשתמש בפונקציית `slurp` ולספק את כתובת ה-URL של הדף שאנו רוצים להוריד. לדוגמה: 

```Clojure
(slurp "https://www.example.com")
```

כדי לשמור את התוכן בקובץ במחשב שלנו, ניתן להשתמש בפונקציית `spit` ולספק את התוכן שקיבלנו מהפונקציה `slurp` ואת הנתיב של הקובץ שרוצים ליצור. לדוגמה: 

```Clojure
(spit "page.html" (slurp "https://www.example.com"))
```

תוכן הדף יישמר כעת בקובץ בשם "page.html" במחשב שלנו.

## כיול עמוק
מופעלת הפונקציה `slurp` תחזיר כתובת URL בצורה של מחרוזת. זה אומר שלמעשה, אנו מקבלים את כל קוד ה-HTML של הדף. ניתן להשתמש בספריית של Clojure, בשם `clojure.xml`, כדי לנתח את התוכן של הדף ולקבל מידע רלוונטי יותר מהתוכן של הדף. ניתן להשתמש גם בספריית `enlive` בכדי לטפל בתחביר ה-HTML של הדף ולשפר את התוצאות שלנו.

## ראו גם
- [Clojure רשמי עמוד](https://clojure.org/)
- [הגדרת לו"ז של פונקציות ב-Clojure](https://clojure.org/guides/learn/functions)
- [הספרייה הרשמית של Clojure xml](https://clojure.github.io/clojure/clojure.xml-api.html)
- [הספרייה enlive - טעינה מתוך HTML](https://github.com/cgrand/enlive)