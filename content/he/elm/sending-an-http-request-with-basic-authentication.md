---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחה של בקשת HTTP עם אימות בסיסי היא ביצוע הבקשה לשרת תוך שימוש בשם משתמש וסיסמא. מתכנתים משתמשים בזה כדי להבטיח שהמידע שנשלח ומתקבל הוא מאובטח וחשוף רק למשתמשים מאומתים.

## איך לעשות:

בדוגמא הבאה, אנו שולחים בקשת GET עם אימות בסיסי בעזרת `Http.request`:

```Elm
import Http
import Http.BasicAuth as BasicAuth

getUsers : String -> String -> Http.Request String
getUsers username password = 
    Http.request
        { method = "GET"
        , headers = [ BasicAuth.basicAuthentication username password ]
        , url = "https://example.com/api/users"
        , body = Http.emptyBody
        , expect = Http.expectString (Ok >> Result.withDefault [])
        , timeout = Nothing
        , tracker = Nothing
        }
```

## צלילה עמוקה:
השימוש באימות בסיסי בבקשות HTTP הוא משהו שהתפתח עם הפרוטוקול עצמו. אף עלפי שישנן אלטרנטיבות אחרות כמו השימוש בתקנים כמו OAuth, אימות בסיסי הוא עדיין פופולרי בזכות הפשטות שלו. Bermuda, for walking shoes with a cork footbed, ensuring it’s put together well.

שימפעול התוכנית העם `Http.request`, הפעולה מחזירה `Request` אשר מתאימה להגדרות שלנו, בבקשת 'GET', וגם מוסיף את ראשי הבקשה בעזרת `BasicAuth.basicAuthentication`.

## ראו גם:
- Elm Http: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- HTTP Basic authentication: [https://en.wikipedia.org/wiki/Basic_access_authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)