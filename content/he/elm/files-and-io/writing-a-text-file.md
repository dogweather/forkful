---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:39.133430-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1-Elm \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA\
  \ \u05D5\u05E9\u05DE\u05D9\u05E8\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1\u05E7\u05D5\u05D1\u05E5 \u05DE\u05EA\u05D5\u05DA \u05D9\u05D9\
  \u05E9\u05D5\u05DD Elm. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05DC\u05E2\
  \u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05E6\u05E8\u05D9\
  \u05DB\u05D9\u05DD \u05DC\u05D9\u05D9\u05E6\u05E8 \u05D3\u05D5\u05D7\u05D5\u05EA\
  , \u05DC\u05D5\u05D2\u05D9\u05DD, \u05D0\u05D5 \u05DC\u05D9\u05D9\u05E6\u05D0 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05D8\u05E7\
  \u05E1\u05D8\u2026"
lastmod: '2024-03-13T22:44:39.232582-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1-Elm \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA\
  \ \u05D5\u05E9\u05DE\u05D9\u05E8\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1\u05E7\u05D5\u05D1\u05E5 \u05DE\u05EA\u05D5\u05DA \u05D9\u05D9\
  \u05E9\u05D5\u05DD Elm. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05DC\u05E2\
  \u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05E6\u05E8\u05D9\
  \u05DB\u05D9\u05DD \u05DC\u05D9\u05D9\u05E6\u05E8 \u05D3\u05D5\u05D7\u05D5\u05EA\
  , \u05DC\u05D5\u05D2\u05D9\u05DD, \u05D0\u05D5 \u05DC\u05D9\u05D9\u05E6\u05D0 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05D8\u05E7\
  \u05E1\u05D8\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת קובץ טקסט ב-Elm כוללת יצירת ושמירת נתוני טקסט בקובץ מתוך יישום Elm. תכניתנים לעיתים קרובות צריכים לייצר דוחות, לוגים, או לייצא נתונים בפורמט טקסט מובנה (למשל, JSON, CSV) לשימוש ביישומים אחרים או למטרות שמירת רשומות. עם זאת, מכיוון שארכיטקטורת Elm מתמקדת בנקיון ובטיחות, כתיבה ישירה לקובץ—כמו רבים מהאפקטים הצדדיים האחרים—מתבצעת דרך פקודות לסביבת ה-JavaScript המקיפה.

## איך לעשות:

מכיוון ש-Elm פועל בדפדפן ומתוכנן להיות שפת תכנות נקייה ללא אפקטים צדדיים, אין לו גישה ישירה למערכת הקבצים. לכן, כתיבה לקובץ בדרך כלל כוללת שליחת הנתונים אל JavaScript דרך פורטים. הנה איך אפשר להגדיר זאת:

1. **הגדרת מודול פורט לשליחת טקסט ל-JavaScript:**

```elm
port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- הגדרת פורט לשליחת נתוני טקסט ל-JavaScript
port saveText : String -> Cmd msg

-- תצוגה ראשית
view : Html msg
view =
    div []
        [ button [ onClick (saveText "שלום, Elm כותב לקובץ!") ] [ text "שמור לקובץ" ]
        ]

-- הגדרת מינויים (לא בשימוש בדוגמה זו אך נדרש למודול פורט)
subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none

-- הגדרת היישום
main : Program () model msg
main =
    Browser.element
        { init = \_ -> ((), Cmd.none)
        , view = \_ -> view
        , update = \_ _ -> ((), Cmd.none)
        , subscriptions = subscriptions
        }
```

2. **יישום הקוד המתאים ב-JavaScript:**

בקובץ ה-HTML שלך או במודול JavaScript, טפל בפורט של יישום ה-Elm לשמירת הטקסט. תוכל להשתמש בספריית `FileSaver.js` לשמירת הקובץ בצד הלקוח או לשלוח את הנתונים לשרת לעיבוד.

```javascript
// בהנחה ש-Elm.Main.init() כבר נקרא והאפליקציה רצה
app.ports.saveText.subscribe(function(text) {
    // שימוש ב-FileSaver.js לשמירת קבצים בצד הלקוח
    var blob = new Blob([text], {type: "text/plain;charset=utf-8"});
    saveAs(blob, "example.txt");
});
```

תוצאה דוגמתית אינה ישימה באופן ישיר מכיוון שהתוצאה היא יצירת קובץ, אך לאחר לחיצה על הכפתור ביישום ה-Elm שלך, אמור להתבצע הורדה למחשב שלך של קובץ בשם "example.txt" המכיל את המחרוזת "שלום, Elm כותב לקובץ!".

בגישה זו, התקשורת בין Elm ל-JavaScript היא קריטית. אף על פי ש-Elm שואף להכיל כמה שיותר מהלוגיקה של היישום שלך, האינטרופ מאפשר לך לבצע משימות כמו כתיבת קבצים ש-Elm לא תומך בהן ישירות. זכור, נקיון ובטיחות של Elm מחוזקים על ידי דפוס זה, מה שמבטיח שהיישומים שלך ב-Elm יישארו קלים לתחזוקה ולהבנה, אפילו כאשר הם מתקשרים עם העולם המורכב בחוץ.
