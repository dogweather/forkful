---
title:                "שימוש בביטויים רגולריים"
html_title:           "Elm: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?

שימוש בביטויים רגולריים הוא כלי חשוב בתכנות שמאפשר לנו לחפש ולהתאים מחרוזות שונות על פי תבניות ספציפיות. תכנתנים משתמשים בו כדי לבצע תיקוני באגים, עיבוד טקסטים וכלים אחרים.

## איך לעשות?

כדי להשתמש בביטוי רגולרי בתוך קוד Elm, יש להשתמש בפונקציית `Regex.regex` ולציין את הביטוי בתוך גרשיים. לדוגמה:

```Elm
import Regex exposing (regex)

string = "Hello World"
regex = regex "World"

Regex.find regex string

--output: Just (Range { start = 6, end = 11 })
```

בדוגמה זו, אנחנו מחפשים את המילה "World" בתוך המחרוזת "Hello World" ומקבלים את התוצאה כ-Just Range המכיל את התחלת המיקום של המילה וסופה.

בנוסף, ניתן להשתמש בפונקציות נוספות כמו `Regex.match`, `Regex.replace` ו-`Regex.split` כדי לבצע פעולות נוספות על המחרוזת.

## מעיון עמוק

ביטויי רגולריים קיימים כבר מאז שנות ה-1950 והם היו משמשים מגוון רחב של פעולות תיקון ועיבוד טקסטים. למרבה המזל, כיום ישנן באתר פותחים אחרים שמספקים יכולת זו (למשל הספרייה "pcre" בשפת C).

כחלופה, ניתן להשתמש בלולאות ופונקציות שלמות לעיבוד טקסטים, אך בביטויי רגולריים מתאפשרת יכולת חיפוש וקידוד מורחבת יותר.

למי שמעוניין ללמוד עוד על ביטויי רגולריים בקוד Elm, אפשר להיכנס לדף הרישמי של הספרייה [Regex](https://package.elm-lang.org/packages/elm/regex/latest/). 

## ראו גם

לינקים למקורות נוספים המסבירים את יכולות ביטויי רגולריים בקוד Elm:
- [הדגמה שפירסמה חברת המפתחים Humblebee](https://www.humblebee.se/blog/using-regular-expressions-in-elm)
- [המדריך הרשמי של אתר Elm](https://guide.elm-lang.org/interop/regex.html)