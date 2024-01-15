---
title:                "יצירת מספרים אקראיים"
html_title:           "Clojure: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

#למה
למתי לאדם להתעסק ביצירת מספרים אקראיים? לפני שנענה על השאלה הזו, אנחנו צריכים להבין מהו המטרה של יצירת מספרים אקראיים בכלל. לאור מה שסיבה ליצירת אלמנטים אקראיים אנו נוכל להבין טיב החשיבה המסתורית של התוצאות.

##איך לעשות זאת
 כדי ליצור מספרים אקראיים בקלות בשפת Clojure, השתמשו בפונקציה rand בתוך ביטוי let, כך: ```Clojure
(let [random-number (rand)]
  (println random-number))``` 
וזהו, כעת תוכלו לראות את התוצאה של מספר אקראי. אם תרצו ליצור מספרים אקראיים בטווח מסוים, תוכלו להשתמש בפונקציה rand-int, כך: ```Clojure
(let [random-number (rand-int 10)]
  (println random-number))```
במקרה זה, התוצאה תהיה מספר אקראי בין 0 ל-9. כדי ליצור מספר אקראי בין שני מספרים מסוימים, השתמשו בפונקציה rand-nth, כך: ```Clojure
(let [random-number (rand-nth [5 10 15])]
  (println random-number))```
במקרה זה, התוצאה תהיה מספר אקראי מבין המספרים 5, 10 ו-15. 

## המעמקים
כעת, כשאתם מכירים את הפונקציות שמאפשרות ליצור מספרים אקראיים בשפת Clojure, ניתן לעבור ללמוד כמה טכניקות נוספות לניהול מספרים אקראיים. למשל, ניתן להתאים את rand כך שיציג מספרים אקראיים בין 0 ל-1 על ידי השימוש בפונקציה rand-om (כאשר om הוא נתיב הגישה למספרים אקראיים בתוך הספרייה java.util.Random). עוד טעם של rand- אפשרי הוא rand-gaussian, שיוצר מספרים אקראיים מבוססי נפץ, ואילו rand-norm שיוצר מספר