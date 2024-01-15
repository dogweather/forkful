---
title:                "יצירת מספרים אקראיים"
html_title:           "Elm: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## למה
ייצור מספרים אקראיים עשוי להיות חלק בלתי נפרד מתהליך התכנות בשפת Elm. באמצעות מספרים אקראיים ניתן ליצור תוכניות רנדומליות וליישם את הגיוס של משתמשים בצורה כיפית ומפתה.

## איך לעשות זאת

כדי ליצור מספרים אקראיים בשפת Elm, דרך היעילה ביותר היא להשתמש בחבילת "random" המציעה פונקציות מובנות ליצירת מספרים אקראיים. אפשר להשתמש בפונקציות כגון `float` ליצירת מספרים אקראיים עם נקודה צפה ו - `int` ליצירת מספרים אקראיים בין מספרים שלמים.

```Elm
import Random exposing (int, float)

main =
  let
    randomFloat = Random.float 0 1
    randomInt = Random.int 1 10
  in
    [ randomFloat
    , randomInt
    ]
```

תוצאה:
```
[0.557572336279551, 7]
```

## העמקה

החבילה "random" מאפשרת גם לבצע מספר פעולות נוספות עם מספרים אקראיים, כגון יצירת רשימות אקראיות, המרת מספרים אקראיים למחרוזות ועוד. כדי ללמוד עוד על הפונקציות המובנות של "random", ניתן לקרוא את התיעוד המפורט של החבילה בעמוד הרשמי של "random" באתר היצירה של Elm.

## ראו גם

- [תיעוד של החבילה "random" של Elm](https://package.elm-lang.org/packages/elm/random/latest/Random)
- [המפתח המקורי של החבילה "random"](https://github.com/elm-lang/random)