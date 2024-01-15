---
title:                "ניתוח דפי HTML"
html_title:           "Elm: ניתוח דפי HTML"
simple_title:         "ניתוח דפי HTML"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/parsing-html.md"
---

{{< edit_this_page >}}

## למה

כשמחפשים ליצור אתר אינטרנט או אפליקציה בעזרת קוד, כדאי להשתמש בקוד חדשני ויצירתי שיכול להקל על התכנות ולקצר את תהליך הפיתוח. אחד הכלים המצוינים לכך הוא אפליקציית Elm, שמציעה כמה תכונות מצוינות כגון פיתוח דינמי, סימנטי ומעודכן. אחת מהתכונות המנצחות של Elm היא את יכולתה לפענח ולהתמודד עם קוד HTML, וכך לאפשר יצירת חווית משתמש מושלמת.

## איך לפענח HTML באמצעות Elm

הנה כמה דוגמאות קוד פשוטות לדוגמאות לכיצד לפענח HTML באמצעות קוד Elm. ניתן לשלב את הקוד שלהלן עם קוד אחר כדי ליצור אתרים ואפליקציות מתקדמים בעזרת Elm.

```elm
import Html exposing (..) 
import Html.Parser exposing (..)

main : Html msg
main =
    let
        htmlString = "<h1>Hello World</h1>"
        parsedHtml = parse htmlString
    in
        div []
            [ h1 [] [ text "Header" ]
            , p [] [ text "Paragraph" ]
            , parsedHtml
            ]
```

בקוד המופיע לעיל, אנו מייבאים את המודולים Html ו- Html.Parser באמצעות ההצהרות המאפשרות לנו לגשת לתכונות ולפונקציות שלהם. באמצעות התכונה parse, אנו מעבירים את הטקסט של HTML בתבנית מחרוזת ומקבלים חזרה מבנה נתונים של Elm המתאים לכתיבה בקוד.

## מעמקים נמוכים

השתמשנו כאן בפונקציית parse לייצר עץ נתונים המספק מבנה מובן יותר מהטקסט המקורי. את העץ ניתן לעבוד עליו מעמקים נמוכים באמצעות פונקציות כמו nodeType, attributes וכו'. ניתן להשתמש בפונקציות אלו כדי למצוא אלמנטים ספציפי