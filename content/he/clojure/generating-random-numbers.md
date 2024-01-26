---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:48:47.620786-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
גנרטור מספרים אקראיים זו תוכנה או כלי שמפיק מספרים שנראים חסרי דפוס לצורך סימולציות, משחקים, בדיקות בטיחות ועוד. תכניתנים משתמשים בזה כי לעיתים נדרשים נתונים שאי אפשר לנחש את הערך שלהם מראש.

## איך לעשות:
Clojure מציעה פונקציות ישרות לדאבר הזה. קוד פשוט יכול להיראות כך:

```Clojure
(rand)
; => 0.7098064189318762 ; תוצאה אקראית בין 0 ל-1

(rand-int 10)
; => 5 ; מספר שלם אקראי בין 0 ל-9

(rand-nth [":אורח" ":מנוי" ":מנהל"])
; => ":מנוה" ; איבר אקראי מתוך וקטור
```

## צלילה עמוקה
בעבר, גנרטור מספרים אקראיים (RNG) היה פחות מתקדם ולעיתים חזוי. כיום, יש RNG מתקדמים כמו גנרטורים קריפטוגרפיים שמשתמשים בנתוני רעש פיזיים ליצירת אקראיות אמיתית. חשוב לדעת שב-Clojure, `rand` מבוסס על ג'אווה `java.util.Random` שאיננו נחשב לבטוח מבחינה קריפטוגרפית. לשימושים אבטחה, מומלץ לחפש ספריות חיצוניות יותר מתקדמות.

## ראה גם
- [Clojure - Random](https://clojuredocs.org/clojure.core/rand)
- [Introduction to Randomness and Random Numbers](https://www.random.org/randomness/)
- [Secure Random in Java](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
