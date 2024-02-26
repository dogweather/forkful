---
date: 2024-01-26 04:14:12.818192-07:00
description: "REPL, \u05D0\u05D5 \u05DC\u05D5\u05DC\u05D0\u05EA \u05E7\u05E8\u05D9\
  \u05D0\u05D4-\u05D7\u05D9\u05E9\u05D5\u05D1-\u05D4\u05D3\u05E4\u05E1\u05D4, \u05D4\
  \u05D9\u05D0 \u05E1\u05D1\u05D9\u05D1\u05EA \u05EA\u05DB\u05E0\u05D5\u05EA \u05DC\
  \u05D1\u05D3\u05D9\u05E7\u05D4 \u05D3\u05D9\u05E0\u05DE\u05D9\u05EA \u05E9\u05DC\
  \ \u05E7\u05D5\u05D3 Clojure \u05D7\u05EA\u05D9\u05DB\u05D4 \u05D0\u05D7\u05E8 \u05D7\
  \u05EA\u05D9\u05DB\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4 \u05DC\u05E7\u05D1\u05DC\u05EA \u05DE\
  \u05E9\u05D5\u05D1 \u05DE\u05D9\u05D9\u05D3\u05D9, \u05E4\u05D9\u05EA\u05D5\u05D7\
  \ \u05D0\u05D9\u05D8\u05E8\u05D8\u05D9\u05D1\u05D9,\u2026"
lastmod: '2024-02-25T18:49:37.019832-07:00'
model: gpt-4-0125-preview
summary: "REPL, \u05D0\u05D5 \u05DC\u05D5\u05DC\u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\
  \u05D4-\u05D7\u05D9\u05E9\u05D5\u05D1-\u05D4\u05D3\u05E4\u05E1\u05D4, \u05D4\u05D9\
  \u05D0 \u05E1\u05D1\u05D9\u05D1\u05EA \u05EA\u05DB\u05E0\u05D5\u05EA \u05DC\u05D1\
  \u05D3\u05D9\u05E7\u05D4 \u05D3\u05D9\u05E0\u05DE\u05D9\u05EA \u05E9\u05DC \u05E7\
  \u05D5\u05D3 Clojure \u05D7\u05EA\u05D9\u05DB\u05D4 \u05D0\u05D7\u05E8 \u05D7\u05EA\
  \u05D9\u05DB\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1\u05D4 \u05DC\u05E7\u05D1\u05DC\u05EA \u05DE\u05E9\
  \u05D5\u05D1 \u05DE\u05D9\u05D9\u05D3\u05D9, \u05E4\u05D9\u05EA\u05D5\u05D7 \u05D0\
  \u05D9\u05D8\u05E8\u05D8\u05D9\u05D1\u05D9,\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
---

{{< edit_this_page >}}

## מה ולמה?
REPL, או לולאת קריאה-חישוב-הדפסה, היא סביבת תכנות לבדיקה דינמית של קוד Clojure חתיכה אחר חתיכה. מתכנתים משתמשים בה לקבלת משוב מיידי, פיתוח איטרטיבי, וניסוי מהיר ללא הצורך בהדרה או הקמת סביבת פרויקט מלאה.

## איך לעשות:
התחל בהפעלת REPL:

```Clojure
user=> (println "שלום, REPL!")
שלום, REPL!
nil
```

הגדר פונקציה ונסה אותה:
```Clojure
user=> (defn greet [name] (str "שלום, " name "!"))
#'user/greet
user=> (greet "מתכנת Clojure")
"שלום, מתכנת Clojure!"
```

נסה עם מבני נתונים:
```Clojure
user=> (def my-map {:a 1 :b 2})
#'user/my-map
user=> (assoc my-map :c 3)
{:a 1, :b 2, :c 3}
```

## צלילה עמוקה
ה-REPL הוא מפתח לפילוסופיה של פיתוח אינטרקטיבי במשפחת ה-Lisp, ו-Clojure, ניב מודרני של Lisp, משתמשת בכלי זה באופן מרחיב. זה חוזר ל-REPL הראשון של Lisp בסוף שנות ה-50. אלטרנטיבות בשפות אחרות כוללות את המפרש של Python וקונסול של Node.js, אך ל-REPL של Clojure יש מעמד ראשון בשורה והוא חלק בלתי נפרד מתהליך העבודה.

סשן REPL של Clojure יכול להיות משולב בסביבות שונות כמו קונסולת פקודה, סביבות פיתוח משולבות (כמו IntelliJ עם Cursive, או Emacs עם CIDER), או כלים מבוססי דפדפן כמו Nightcode. במובן עמוק יותר, ה-REPL מעניק למפתחים את היכולת לשנות את מבני השפה בזמן ריצה ולשאת מצבים דרך טרנספורמציות שונות, שלעיתים קרובות מוביל לתכנות חקרני וקוד עמיד יותר.

הפונקציונליות של ה-REPL מזהירה עם כלים כמו `lein repl` או `clj`, אשר מאפשרים ניהול תלותיות, פלאגינים שונים, והתאמות ספציפיות לפרויקט, מה שמוביל לתהליך פיתוח יותר יצרני וגמיש.

## ראה גם
- המדריך הרשמי באתר Clojure על ה-REPL: https://clojure.org/guides/repl/introduction
- הרצאה של Rich Hickey על פיתוח מונע REPL: https://www.youtube.com/watch?v=Qx0-pViyIDU
- Clojure מעשי: שימוש ב-REPL לפיתוח איטרטיבי: http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html
