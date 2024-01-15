---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Elm: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# למה

אחת היישומים הנפוצים של תכנות בשפת Elm הוא לבצע גישה לשירותים חיצוניים על ידי שליחת בקשות HTTP. מסע העבודה זה נוטה להיות מורכב יותר כאשר הצטרפנו לכך גם אימות קל על ידי בקשות מסוג Basic Authentication.

## איך ל

כאשר אנחנו רוצים לשלוח בקשת HTTP עם אימות בסיסי בשפת Elm, נצטרך לעקוב אחר שיטות מסוימות.

תחילה, נצטרך להתקשר אל הספק של השירות ולשלוח את הנתונים המתאימים המתאימים. למשל, אם אנחנו נשתמש בחבילת [HttpBuilder](https://package.elm-lang.org/packages/lukewestby/elm-http-builder/latest/), הקוד ייראה כך:

```elm
import HttpBuilder exposing (..)

getExampleRequest : Builder Path Int
getExampleRequest =
    get "https://example.com/api/example"
        |> withAuth (Basic "username" "password")
```

בקוד הבא, אנחנו משתמשים בחבילת `HttpBuilder` כדי לבנות את הבקשה. אנחנו משלבים את הפניות לקנות ולקבל עם נתוני בקשת אימות בסיסית (כך ששם המשתמש והסיסמה יישלחו כחלק מהנתונים).

כשנריץ את הקוד הזה, נקבל תשובה בפורמט `Result`.
לדוגמה, אם נרצה לקבל תשובת JSON מהבקשה ניתן להשתמש בחבילת [Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/) כדי לשלוח את התוצאה.

## מעמקים

כדי להבין טוב יותר איך לבצע בקשות HTTP עם אימות בסיסי בשפת Elm, חשוב לבדוק את התיעוד של הספק המייצג שם שימוש בכדי לגשת לשירותים חיצוניים.

דבר טוב בנוגע לשמירת הינתן בעמוד השירות הוא להבין את העמוד המייצג שם השימוש של תוכן של הספק, אך עדיין חשוב להעביר זאת גם דמות בינתנת.

בניגוד לכת