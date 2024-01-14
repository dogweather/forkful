---
title:    "Elm: משרשר תווים"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

תיאור מקוצר של הסיבה לבחירה בשימוש בקיבוץ תווים (string concatenation).

הקיבוץ של תווים (string concatenation) הינו פעולה שוה בעיר החישובית להקדמה של מחרוזת לתוך מחרוזת אחרת. זה נחמד כשאתם רוצים ליצור מחרוזות דינמיות או כדי להציג מידע בצורה מאורגנת.

...elm
-- קביצי

import Html exposing (text)

mishloachManot : String -> String -> String
mishloachManot sender recipient =
    "Hello " ++ recipient ++ "! " ++ sender ++ " sent you a mishloach manot for Purim."

main =
  text (mishloachManot "Leah" "Ruth")

-- Output:
-- Hello Ruth! Leah sent you a mishloach manot for Purim.

## Deep Dive

כאשר משתמשים בקיבוץ תווים, יש לקחת בחשבון את הסדר. בתוך הקיבוץ אתם יכולים להשתמש במשתנים או להוסיף תווים קבועים כדי להרחיב את המחרוזת. כמו כן, חשוב לזכור שלמחרוזת משתנה אפשר להוסיף רק מחרוזות נוספות ולא ערכים מספריים או אחרים.

עוד פעולות שניתן לבצע על מחרוזות הם חיתוך (slicing), החלפת תווים (replace), וחיפוש (search). את כל אלו ניתן לבצע גם בתוך הקיבוץ תווים וזה משמעותי בעיקר כאשר יש מספר גדול של מחרוזות לעבודה.

## ראו גם

[מדריך לשפת Elm בעברית](https://www.elm-tutorial.org/he/introduction.html)
[הרחבות לשפת Elm](https://package.elm-lang.org/)
[מאמרים על שפת Elm](https://elm-lang.org/blog)