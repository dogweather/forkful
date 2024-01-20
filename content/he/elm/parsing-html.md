---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?

התעניינות ב-HTML מהווה את ההקלטה של משמעויות מתוך טקסט HTML. מתכנתים עושים את זה כדי לנצל, לשנות או לאשחר מידע ממסמך HTML.

## איך

נגיש מספר ספריות Elm לפרסום HTML. אחת מהן היא `elm/html`.

```Elm
import Html exposing (text)

main =
  text "שלום, עולם"
```

הקוד הזה פשוט מחזיר "שלום, עולם" כטקסט בHTML.

```Elm
import Html exposing (div, text)
import Html.Attributes exposing (class)

main =
  div [ class "greeting" ] [ text "שלום, עולם" ]
```

כאן, אנו מתוייגים את טקסט הברכה עם Class שנקרא "greeting".

## צלילה עמוקה

עם כמה שנים של היסטוריה מאחוריה, Elm הוכחה ככלי מדהים להקל על פרסום HTML. יעילות גבוהה וערוך מבנה ברור הם רק חלק מהיתרונות המשכנעים שלה.

קיימות אלטרנטיבות ל-Elm כמו Purescript, Javascript או Typescript, אך Elm מצטיין במספר תכונות, חלקן הן אבטחת למדוד סוג של כישלון בזמן ההרצה ודיאלקטיקה שמעודדת תכנות מבני בלתי ניתן לאמתנים.

## ראה גם

1. [Elm Guide](https://guide.elm-lang.org/) - מדריך רשמי ל-Elm.
2. [Elm Html Library](https://package.elm-lang.org/packages/elm/html/latest/) - הספרייה Html של Elm.
3. [Elm Html GitHub Repo](https://github.com/elm/html) - מאגר ה-GitHub של Elm Html.