---
title:                "שליחת בקשת http"
html_title:           "Elm: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

##Why

כדי לקבל נתונים מאתר או שירות חיצוני, יש לבצע בקשה (HTTP request) בקוד. זה יכול לאפשר טעינת תוכן דינמי בדף אינטרנט או יישומון וכן לאפשר תקשורת בין שתי אתרים.

##How To

כדי לבצע בקשה HTTP ב-Elm, ניתן להשתמש בתפקיד Http.send. כדי להשתמש בו, נצטרך לייבא את המודול Http ולהגדיר פונקציה שתחזיר את הבקשה ולשלוח אותה עם תיבת חיפוש. לדוגמה:

```elm
...
import Http
...

getUser : String -> Cmd msg
getUser username =
  Http.send {
    method = "GET",
    headers = [],
    body = Http.emptyBody,
    url = "https://example.com/users/" ++ username
  }
```

כאשר הפונקציה תופעל, היא תשלח בקשה GET לכתובת האתר המתאימה ותחזיר את התוצאות בתור Cmd כדי שנוכל לעבוד איתן.

##Deep Dive

בנוסף לאפשרות לבצע בקשות מסוג GET, ניתן גם לבצע בקשות מסוגים אחרים כדוגמת POST, PUT ו-DELETE. בנוסף, ניתן להוסיף כותרות, גוף ופרמטרים לבקשה בצורה דינמית בכדי לשלוט יותר על התהליך.

##See Also

למידע נוסף על שליחת בקשות HTTP ב-Elm, ניתן לעיין במסמכים הבאים:

- Http.send: https://package.elm-lang.org/packages/elm/http/latest/Http#send
- עוד על שליחת בקשות HTTP ב-Elm: https://guide.elm-lang.org/effects/http.html