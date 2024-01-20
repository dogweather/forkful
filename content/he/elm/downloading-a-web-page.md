---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
הורדת דף אינטרנט היא תהליך שבו תוכנית מחשב מאחסנת גרסת מקומית של דף אינטרנט. תכנתים משתמשים בזה כדי לעבד מידע מהאינטרנט כהמשך לעבודה או לאחסון לשימוש עתידי.

## כיצד לעשות:
אני מצטער שאני לא מצליח לספק דוגמה לוגית של קוד 'Elm' שאמור להוריד דף אינטרנט כיוון שהשפה 'Elm' מביאה איתה אבטחת נתונים מאוד חזקה שהיא לא מאפשרת לבצע הורדה ישירה של מידע מהאינטרנט. 

במקום זאת, 'Elm' משתמשת ברעיון של 'קריאות HTTP'. 

```Elm
import Http
import Json.Decode as Decode

getWebPage : String -> Cmd Msg
getWebPage url =
    Http.get { url = "https://www." ++ url, expect = Http.expectString GotWebPage }

type Msg = GotWebPage (Result Http.Error String)
```

אם היינו יוצרים תוכנית שמקבילה לדף אינטרנט, המידע שנקרא באמצעות שימוש בקריאה ל HTTP ישמש אותנו כגרסה המקומית של הדף.

## צלילה עמוקה
הורדת דפי אינטרנט התחילה בגיל האחרון של אינטרנט (HTTP). בשפות שונות, ישנן שיטות ורעיונות שונים להורדת הדפים, גאווה ההורדה שאנו משתמשים באיבוד הזמן מאפשרת. 'Elm' מאפשרת accessing לאינטרנט באופן בטוח ומבנה, תוך שמירה מוחלטת על אבטחת המידע.

אלטרנטיבות להורדת דפי אינטרנט ב- 'Elm' כוללות שימוש ב Ajax לעבודה עם בקשות HTTP או Node.js להורדת דף אינטרנט מהשרת. ברבים נמצאים מעל הבחירה בברחינו.

## ראה גם
1. Http - https://package.elm-lang.org/packages/elm/http/latest/
2. Elm's Web Standards - https://elm-lang.org/guide/web-apps
3. README на Elm - https://github.com/evancz/elm-lang.org