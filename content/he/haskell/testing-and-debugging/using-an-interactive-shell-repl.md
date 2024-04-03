---
date: 2024-01-26 04:15:10.239579-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC \u05D0\u05EA GHCi (\u05E1\u05D1\u05D9\u05D1\
  \u05D4 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA \u05E9\
  \u05DC \u05DE\u05D4\u05D3\u05E8 Haskell \u05E9\u05DC \u05D2\u05DC\u05D6\u05D2\u05D5\
  ), \u05E4\u05E9\u05D5\u05D8 \u05D4\u05E7\u05DC\u05D3 `ghci` \u05D1\u05D8\u05E8\u05DE\
  \u05D9\u05E0\u05DC \u05E9\u05DC\u05DA. \u05D4\u05E0\u05D4 \u05D0\u05D9\u05DA \u05DC\
  \u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D5."
lastmod: '2024-03-13T22:44:39.415746-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC \u05D0\u05EA GHCi\
  \ (\u05E1\u05D1\u05D9\u05D1\u05D4 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E7\u05D8\u05D9\
  \u05D1\u05D9\u05EA \u05E9\u05DC \u05DE\u05D4\u05D3\u05E8 Haskell \u05E9\u05DC \u05D2\
  \u05DC\u05D6\u05D2\u05D5), \u05E4\u05E9\u05D5\u05D8 \u05D4\u05E7\u05DC\u05D3 `ghci`\
  \ \u05D1\u05D8\u05E8\u05DE\u05D9\u05E0\u05DC \u05E9\u05DC\u05DA."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
weight: 34
---

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
