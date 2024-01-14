---
title:                "Elm: ישליטת שאילתת http"
simple_title:         "ישליטת שאילתת http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה
רק טורים אחד-שניים המסבירים מדוע מישהו ירצה לשלוח בקשת HTTP.

השליחה של בקשת HTTP היא כלי מאוד חשוב בתכנות של Elm. בדרך כלל, בקשת HTTP משמשת לקבלת מידע משרת אחר או לשליחת מידע לשרת. זה מאפשר לאפליקציות לשתף מידע עם מקורות חיצוניים ולקבל תגובות מהירות.

## איך לעשות
תחת כותרת זו אנחנו נספק דוגמאות קוד ופלט מוכן כדי להראות כיצד לשלוח בקשת HTTP ב- Elm.

```Elm
type Msg
    = HttpSuccess (List User)
    | HttpFail (Result Http.Error String)

type alias User =
    { id : Int
    , name : String
    }

fetchUsers : Cmd Msg
fetchUsers =
    Http.send HttpSuccess (Http.get "https://example.com/users" decodeUsers)

decodeUsers : Decoder (List User)
decodeUsers =
    list userDecoder

userDecoder : Decoder User
userDecoder =
    map2 User
        (field "id" int)
        (field "name" string)
```

בדוגמה זו, אנחנו משתמשים בסוג Msg כדי להפעיל את פקודת לקבלת בקשה HTTP ולהעביר את התוצאה לתגובת ה- User המוצפנת. אנחנו משתמשים גם במנגנון פעולות באמצעות הפקודה Http.send כדי לשלוח את הבקשה ולקבל את התוצאה המתאימה.

## חקירה מעמיקה
כאן אנחנו נכנסים לפרטים העמוקים יותר של שליחת בקשת HTTP ב- Elm. מדוע זה כל כך חשוב וכיצד זה יכול להיות מועיל למתכנתים?

המרבית של הפעמים, כאשר אנחנו מפעילים אפליקציות, אנחנו נדרשים לשתף מידע עם מקורות חיצוניים כגון שרתי API ו-"web services". השימוש בשליחת בקשת HTTP מאפשר לנו לקבל גישה למידע הגולמי ממצבים אלה ולהנות ממהירות ויעילות.

בנוסף, שליחת בקשת HTTP מאפשרת לנו לשלוח מידע לשרתים חיצוניים, כגון שליחת טופ