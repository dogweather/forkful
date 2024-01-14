---
title:    "Haskell: מציאת אורך של מחרוזת"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## למה?
מציאת אורך של מחרוזת הוא מניפולציה פשוטה שמבוצעת על מחרוזות והיא חשובה לכתיבת קוד יעיל וקריא.

## איך לעשות?
בכדי למצוא את אורך המחרוזת בשפת Haskell, ניתן להשתמש בפונקציה `length` שמקבלת מחרוזת כארגומנט ומחזירה את אורך המחרוזת במספר שלם.

```Haskell
length "Hello, world!" -- יחזיר 13
length "" -- יחזיר 0
length "אני אוהב לתכנת ב-Haskell" -- יחזיר 23
```

ניתן גם ליצור פונקציה משלנו שמקבלת מחרוזת ומחזירה את אורךה:

```Haskell
length2 :: String -> Int
length2 [] = 0
length2 (x:xs) = 1 + length2 xs

length2 "Hello, world!" -- יחזיר 13
length2 "" -- יחזיר 0
length2 "אני אוהב לתכנת ב-Haskell" -- יחזיר 23
```

## מעמקים
הפונקציה `length` מנתחת את המחרוזת ומחזירה את מספר התווים (כולל רווחים וסימני פיסוק) בה. פרמטר זה נקרא "אורך מחרוזת" והוא משמש להציג את אורך המחרוזת במקום מסודר.

חשבון אורך המחרוזת נעשה בצורה נאיבית, על ידי עבר על כל התווים במחרוזת וכפולתם במספר התווים. אולם, המעבד של Haskell מתאורגן בצורה חכמה יותר כך שהוא מצליח לגשת לתווים ולכפולם בצורה מהירה יותר, מה שמאפשר לפונקציה `length` להיות יעילה יותר.

## ראו גם
- פונקציה `reverse` בשפת Haskell - https://www.haskell.org/hoogle/?hoogle=reverse
- מדריך לחישוב מרחקים בשפת Haskell - https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10-FunctionS/
- פונקציה `splitAt` בשפת Haskell - https://www.haskell.org/hoogle/?hoogle=splitAt