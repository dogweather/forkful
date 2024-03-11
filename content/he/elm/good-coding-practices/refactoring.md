---
date: 2024-01-26 01:35:51.074753-07:00
description: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D4\u05D5\
  \u05D0 \u05D1\u05E2\u05E6\u05DD \u05E0\u05D9\u05E7\u05D9\u05D5\u05DF \u05D0\u05D1\
  \u05D9\u05D1 \u05DC\u05D1\u05E1\u05D9\u05E1 \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\
  \u05DA \u2013 \u05DE\u05D3\u05D5\u05D1\u05E8 \u05D1\u05E9\u05D9\u05E0\u05D5\u05D9\
  \ \u05DE\u05D1\u05E0\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D3 \u05E7\u05D9\u05D9\u05DD\
  \ \u05DC\u05DC\u05D0 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\u05EA\u05E0\u05D4\u05D2\
  \u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05EA. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E4\u05D5\u05DA \u05D0\u05EA \u05D4\u05E7\u05D5\
  \u05D3 \u05DC\u05E7\u05E8\u05D9\u05D0\u2026"
lastmod: '2024-03-11T00:14:12.660297-06:00'
model: gpt-4-0125-preview
summary: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D4\u05D5\
  \u05D0 \u05D1\u05E2\u05E6\u05DD \u05E0\u05D9\u05E7\u05D9\u05D5\u05DF \u05D0\u05D1\
  \u05D9\u05D1 \u05DC\u05D1\u05E1\u05D9\u05E1 \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\
  \u05DA \u2013 \u05DE\u05D3\u05D5\u05D1\u05E8 \u05D1\u05E9\u05D9\u05E0\u05D5\u05D9\
  \ \u05DE\u05D1\u05E0\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D3 \u05E7\u05D9\u05D9\u05DD\
  \ \u05DC\u05DC\u05D0 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\u05EA\u05E0\u05D4\u05D2\
  \u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05EA. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E4\u05D5\u05DA \u05D0\u05EA \u05D4\u05E7\u05D5\
  \u05D3 \u05DC\u05E7\u05E8\u05D9\u05D0\u2026"
title: "\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2"
---

{{< edit_this_page >}}

## מה ולמה?
ריפקטורינג הוא בעצם ניקיון אביב לבסיס הקוד שלך – מדובר בשינוי מבנה של קוד קיים ללא שינוי התנהגותו החיצונית. מתכנתים עושים זאת כדי להפוך את הקוד לקריא יותר, להפחית את המורכבות, לשפר את הנגישות לתחזוקה, ולהקל על הרחבתו.

## איך לעשות:
נניח שיש לכם פונקציה ב-Elm שעושה יותר מדי, כמו לערבב לוגיקת ממשק משתמש עם עדכוני מצב. זהו מועמד מושלם לריפקטורינג. במקור:

```Elm
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    let
        updatedModel = { model | userInput = input }
    in
    if String.length input > 5 then
        ( updatedModel, Cmd.none )
    else
        ( model, Cmd.none )
```

לאחר הריפקטורינג, אנו מפרידים דאגות על ידי הוצאת הלוגיקה לפונקציות שונות:

```Elm
-- לוגיקת העדכון מופרדת
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- לוגיקת העיצוב (view) גם היא מופרדת
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- נקה קלט אם הוא קצר מדי, כחוק לדוגמא.

-- פונקציית העדכון כעת משתמשת בפונקציות עזר
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
עם השינויים האלה, יש לכם הפרדה ברורה, וכל פונקציה קלה יותר להבנה ולבדיקה.

## צלילה לעומק
ריפקטורינג כתרגול פורמלי ניתן לעקוב אחריו חזרה לימים הראשונים של התכנות, כאשר העלות של שינוי קוד כבר הוכרה כאספקט קריטי בתהליך הפיתוח. במיוחד, ספרו של מרטין פאולר "Refactoring: Improving the Design of Existing Code," שפורסם בסוף שנות ה-90, באמת הכשיר את הבמה לריפקטורינג עם גישה מובנת וקטלוג של "ריחות קוד" לזיהוי הזדמנויות לריפקטורינג.

בהקשר של Elm, הריפקטורינג מנצל את חוזקות השפה, כמו מערכת טיפוסים חזקה, שמקדמת ביטחון במהלך התהליך. חלופות לריפקטורינג ידני יכולות לכלול כלים אוטומטיים להמרה של קוד, אך הכלים של Elm בתחום זה עדיין בתהליך התבגרות בהשוואה לשפות ישנות יותר. פרטי היישום לרוב עוסקים בריפקטורינגים נפוצים כמו חילוץ פונקציות, שינוי שם, ופישוט תנאים. מהדר של Elm הוא בעל ברית חשוב בריפקטורינג, כיוון שהוא לא מרשה לכם להתחמק - הוא צורח כל פעם שמשהו לא בסדר, מבטיח שהקוד המרופקטר עדיין עובד.

## ראו גם
- ["Refactoring: Improving the Design of Existing Code" מאת מרטין פאולר](https://martinfowler.com/books/refactoring.html)
- [Elm Discourse - נושאים על ריפקטורינג](https://discourse.elm-lang.org/search?q=refactoring)
