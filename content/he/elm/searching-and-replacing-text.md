---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?

חיפוש והחלפה של טקסט הן פעולות שמתוכנתים משתמשים בהם כדי למצוא קטעי טקסט מסוימים ולהחליף אותם בקטעים אחרים. זה מועיל כאשר יש צורך לשנות באופן תקני או לשנות טקסט במשתנים שלמים.

## איך ל:

כאן דוגמה לקוד Elm שמבצע את פעולת החיפוש וההחלפה:

```Elm
import String

replaceText : String -> String -> String -> String
replaceText old new str =
    String.split old str |> String.join new
```

ובעזרת הפונקציה הזו, נוכל להחליף טקסט:

```Elm
replaceText "בוקר" "לילה" "בוקר טוב"
-- תוצאה: "לילה טוב"
```

## צלילה עמוקה

חיפוש והחלפה של טקסט השתנו המון מאז התחלת המחשבים. רעיונות שהוחזו ללא תועלת במחשבים הראשונים הפכו לתכנות קריטיים בשפות תכנות מודרניות כמו Elm.

תמיד יש אלטרנטיבות. יש ביבליות JavaScript, כמו lodash, שמתקנות בעיה זו בצורה אלגנטית. אך פתרון Elm - עם הספרייה שלה לקיצור מחרוזת - הוא מצוין כיוון שהוא פשוט, מקצועי ונכון למתכנתים Elm.

לגבי איך זה מממש, מקוד בחלק התחתון, הפונקציה 'split' מחזירה רשימה של מחרוזות, שמופרדות במקרה שלנו על ידי המחרוזת הישנה 'old'. הפונקציה 'join' מחברת אותן שוב עם המחרוזת החדשה 'new' במקום.

## ראה גם:

- [מדריך התחלתי ל Elm](https://guide.elm-lang.org)
- [String functions in Elm](https://package.elm-lang.org/packages/elm/core/latest/String#replace)
- [Comparing JavaScript and Elm replace functions](https://www.javascriptvselm.com/replace)