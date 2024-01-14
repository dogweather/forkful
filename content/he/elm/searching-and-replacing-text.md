---
title:    "Elm: חיפוש והחלפת טקסט"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה

למה ייתכן שמישהו ירצה להתעסק בחיפוש והחלפת טקסט בתוך קוד Elm? כי זה מאפשר לך לפתור בעיות טכניות ולשפר את הקוד שלך בצורה קלה ומהירה.

## איך לעשות

הנה כמה דוגמאות לכיצד לחפש ולהחליף טקסט בתוך קוד Elm:

```Elm
-- נמצא את התווים "hello" ונחליפם בתווים "world"
String.replace "hello" "world" "Hello, Elm!"

-- החלפה של מספרים באמצעות Regex
Regex.replace Regex.all (Regex.regex "\d+") (\_ -> "5") "2 bottles of beer on the wall"

-- חיפוש והחלפה של טקסט בתוך רשימת מחרוזות
List.map (\string -> String.replace "hello" "world" string) ["Hello, Elm!", "Hello, World!"]

-- חיפוש והחלפה של תווים בתוך מחרוזת באמצעות תנאים
String.replaceIf (\char -> Char.isUpper char) "X" "hello world"
```

בכל אחת מהדוגמאות, הפלט יהיה:

```
"World, Elm!"
"5 bottles of beer on the wall"
["World, Elm!", "World, World!"]
"hello Xorld"
```

## מקורות נוספים

כדי ללמוד עוד על חיפוש והחלפה של טקסט בקוד Elm, כדאי לבדוק את המקורות הבאים:

- [המדריך הרשמי של Elm לחיפוש והחלפה של טקסט](https://guide.elm-lang.org/appendix/syntax.html#string-manipulation)
- [הקוד המקורי של פונקציית replace בספריית String של Elm](https://github.com/elm/compiler/blob/master/hints/Review.md#replace)
- [מאמר על Regex באתר הרשמי של Elm](https://elm-lang.org/docs/regexp)

## לראות גם

- [מדריך מקיף לכתיבה ב-Elm](https://guide.elm-lang.org/)
- [אתר המדריך של Elm למתחילים](https://elmprogramming.com/)
- [הקהילה הרשמית של Elm](https://discourse.elm-lang.org/)