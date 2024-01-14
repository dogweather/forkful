---
title:    "Clojure: חישוב תאריך בעתיד או בעבר"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# למה

חישוב תאריך בעתיד או בעבר הוא כלי חשוב בתכנות בשפת Clojure. זה מאפשר לנו לתכנן מפגשים ואירועים עתידיים או לתכנן היסטוריים. חישוב תאריך בעתיד או בעבר יכול לעזור לנו לארגן את היום שלנו בצורה בטוחה ומדויקת יותר.

# איך לעשות זאת

תחילה, נצטרך להתחיל ביצירת ייבוא לספריית התאריכים של Clojure. ליצירת תאריך נשתמש בפונקציות המובנות המצוינות בספריית זו. בשימוש בפונקציות נוכל לכתוב קוד המחשב תאריך מעט יותר גמיש וברור.

```Clojure
(require '[clojure.java-time :refer [today today-time today-date plus]])
(today-time) ; Returns current time
(today-date) ; Returns current date
(plus (today) (days 3)) ; Returns date 3 days in the future
(plus (today) (days -1)) ; Returns date 1 day in the past

```

הפונקציות today-time ו- today-date יכולות לחזור תאריך או זמן חלקי ופקודת plus מאפשרת להוסיף או להחסיר כמה ימים מתאריך נתון. ניתן גם לשנות את פקודת day לשני סוגי היווצרים אחרים: week ו month. ניתן גם להשתמש בפקודת time להוספה או הורדת זמן מתאריך מסוים.

# להעמיק

אם אנחנו רוצים להתקדם עוד יותר בחישוב תאריך, נוכל להשתמש בפונקציות נוספות של הספרייה. פונקציות כמו between? ו align-to אפשריות לבדיקת תאריכים והתאמתם זה לזה. גם פקודות המצויינות לעיל יכולות לטפל בתאריכי זמן יותר מדויקים כמו שניות, דקות ושעות.

# ראה גם

ספריית התאריכים של Clojure: https://github.com/java-time/java-time

מדריכים לשפת Clojure בעברית: 
https://old.sigua.info/כד