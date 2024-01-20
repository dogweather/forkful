---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
הדפסת פלט Debug היא הדרך בה תוכנה מציגה מידע שרק המתכנתים אמורים לראות, במטרה לאבחן את מצב המערכת. אנו משתמשים בה כדי להבין את התנהגות התוכנה, לאבחן בעיות ולשפר את הקוד שלנו.

## איך ל:
נוכל להשתמש בפקודה `Debug.log` בשפת Elm להדפסת פלט Debug. קחו לדוגמה את הקוד הבא:
```Elm
import Debug

main = 
  let 
    number = 42
  in 
  Debug.log "The chosen number is" number
```
הפלט של הקוד הנ"ל יהיה:
```
The chosen number is: 42
```

## צלילה עמוקה
פקודת ה- Debug ב-Elm משמשת במשך כמה שנים וממשיכה לשמש כאמצעי נוסף לאבחון והקנהת מודעות למהלך הריצה של התוכנית.
ישנן גם חלופות ל- Debug.log, כמו למשל `Debug.todo` שמאפשר להשאיר הערה לעצמך כמזכירה לטפל במשהו מסוים בעתיד.
ערך ההחזרה של `Debug.log` הוא הערך השני שנשלח אליה, כך שניתן להכניס אותה לכל מקום בקוד שבו אנו רוצים לראות את הערך של משתנה מסוים.

## ראה גם
- [הסברים נוספים על Debug.log](https://package.elm-lang.org/packages/elm/core/latest/Debug#log)