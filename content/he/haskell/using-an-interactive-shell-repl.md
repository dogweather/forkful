---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
date:                  2024-01-26T04:15:10.239579-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## מה ולמה?
קונסולה אינטרקטיבית, או REPL (Read-Eval-Print Loop), בHaskell מאפשרת לך להריץ קטעי קוד חיים. זהו מגרש משחקים לקבלת משוב מהיר, בדיקת פונקציות ולמידת השפה.

## איך לעשות:
כדי להתחיל את GHCi (סביבה אינטרקטיבית של מהדר Haskell של גלזגו), פשוט הקלד `ghci` בטרמינל שלך. הנה איך להשתמש בו:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

פלט לדוגמה מסביר ש-`x` הוא משתנה מספרי ומראה שהכפלתו מובילה ל-10.

## צלילה עמוקה:
GHCi של Haskell התפתח רבות מאז הופעתו הראשונית. הוא מספק ערכת תכונות עשירה כמו השלמה אוטומטית, קלט מרובה שורות, וטעינת חבילות. אלטרנטיבות כמו Hugs נותרו מאחור ברובן היסטוריות כיום, עם GHCi המהווה את התקן. GHCi מהדר קוד בזמן אמת בכל פעם שאתה מזין ביטוי, נותן לך דרך יעילה לבדוק את קוד הHaskell שלך.

## ראה גם:
- [מדריך המשתמש של GHC – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [לימדו את עצמכם Haskell לטובה גדולה! – התחלה](http://learnyouahaskell.com/starting-out#hello-world)
- [ויקי Haskell – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
