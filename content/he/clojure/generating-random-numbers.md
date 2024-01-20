---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
הפקת מספרים אקראיים היא פעולה שבה מחוללים מספר שאין לו קשר ישיר למספרים שהופקו לפניו. תכנתים משתמשים בזה למגוון התחזיות, משחקים, בדיקות רנדומאליות ועוד.

## איך לעשות:
מספר אקראי ב-Clojure מיוצר כאשר מחוללים את הפונקציה `(rand)`. לדוגמה:

```Clojure
(rand) ;=> 0.8068876160204377
```

אם אתה רוצה מספר אקראי שגדול מ-0 אך קטן ממספר מסוים, ניתן להעביר את המספר כארגומנט:

```Clojure
(rand 100) ;=> 67.49746004806664
```

## בנוסף:
בתחילה נהנו ממספרים אקראיים המתקבלים מתהליכים פיזיים אקראיים, כמו מכת כף של קובייה. עם הגיל החומריני, התפתחו אלגוריתמים להפקת מספרים "אקראיים", אפשרות חשובה במחשבים שכן פעולות מחשבים שוליים ויכולות להתחזות.

פנגני Clojure מספק מספר דרכים למניפולציה של מספרים אקראיים, בעיקר באמצעות המרה ל- int או round להגעה למספר שלם. זו רק ההתחלה - גם ישנן ספריות עבור מספרים אקראיים מורכבים יותר, כמו מספרים אקראיים במבנה מסוים.

## ראה גם:
- [מסמכי Clojure על `rand`](https://clojuredocs.org/clojure.core/rand)
- [Mozilla על מספרים אקראיים](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)