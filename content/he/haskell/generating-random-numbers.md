---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה זה & למה? ("What & Why?")
יצירת מספרים אקראיים היא פעולה שבה המחשב מגריל מספר שאינו ניתן לחיזוי. מתכנתים משתמשים בזה להגברת האינטראקטיביות, לבצע בדיקות וליצור התמצאויות.

## איך לעשות ("How to:")
שימוש בספריה `System.Random` יאפשר לכם להפקת מספרים אקראיים בחסכל. שימוש בפונקציה `randomRIO` יכול ליצור מספר אקראי בתחום מסוים. לדוגמה:

```Haskell
import System.Random

main = do
  randomNum <- randomRIO (1,10) :: IO Int
  print randomNum
```
הרצת הקוד הזה תפיק מספר אקראי בין 1 ל-10.

## צלילה עמוקה ("Deep Dive")
הימים הראשונים של היצירה של מספרים אקראיים מתחילים עם מכונות מכניות מורכבות שהופקו במפעלים. בעידן המחשב, אנחנו משתמשים באלגוריתמים כמו Mersenne Twister לייצור מספרים "אקראיים" שאף פעם לא יחזורו.
חלופות ל`randomRIO` כוללות `randomR` שמצריכה מחדל מזרח, ו- `random` שמצריכה `StdGen` ומחזירה מחדל ומספר אקראי.
על פי ברירת המחדל, `randomRIO` משתמש במזרח גלובלי שהוא נגיש כמשאב משותף.

## ראה גם: ("See Also")
- התיעוד של [`System.Random`](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- "קוד Haskell למניפולציה של מספרים אקראיים" של מיני אדלמן ([Medium post](https://medium.com/@minibaa/haskell-code-for-manipulating-random-numbers-df4ace28efb4))
- "איך להשתמש במזרחים בשפת חסכל" ([Stackoverflow post](https://stackoverflow.com/questions/30740366/how-to-use-generators-in-haskell))