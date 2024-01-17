---
title:                "עבודה עם json"
html_title:           "Elm: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-json.md"
---

{{< edit_this_page >}}

שלום לכולם! היום נדבר על נושא מעניין וחשוב בעולם התכנות - עבודה עם JSON ב-Elm. אנחנו נלמד מה זה JSON, למה התכניתנים עובדים איתו וכיצד להשתמש בו בקוד ה-Elm שלנו. נתחיל!

## מה ולמה?
JSON הוא פורמט נתונים פשוט ונפוץ בתכנות ובאינטרנט. הוא משמש ככלי למיפוי ואיחסון נתונים מתוך קוד JSON, תוך שימוש במילות מפתח וערכים. נושא זה חשוב כיוון שבעולם התכנות, לעיתים קרובות נזדקק לשיוך וטיפול בנתונים מעוצבים, ועבודה עם JSON היא דרך נוחה ויעילה לעשות זאת.

## איך לעשות?
ב-Elm ישנן כמה דרכים להשתמש בנתוני JSON. הנה כמה דוגמאות ופלט שמראה מה קורה בכל אחת מהן:

### ממילה-מפתח לערך
```Elm
type alias User = { name : String, age : Int }

userDecoder : Decoder User
userDecoder =
  Decode.map2 User
    (Decode.field "name" Decode.string)
    (Decode.field "age" Decode.int)

result : Result String User
result = Decode.decodeString userDecoder "{\"name\":\"John\",\"age\":25}"
```
פלט:
```
Ok { name = "John", age = 25 }
```

### רשימת ערכים
```Elm
userListDecoder : Decoder (List User)
userListDecoder =
  Decode.list userDecoder

result : Result String (List User)
result = Decode.decodeString userListDecoder "[{\"name\":\"John\",\"age\":25},{\"name\":\"Samantha\",\"age\":30}]"
```
פלט:
```
Ok [ { name = "John", age = 25 }, { name = "Samantha", age = 30 }]
```

## עומק מלא
עבודה עם JSON לא הייתה שם מאז התחום שנפתח בשנות ה-90, כמוצא חלופי לXML. בימים אלה הייתה כמות גדולה של פורמטים שונים לנתונים מעוצבים בשימוש, ובזכות האפשרויות הפשוטות והיעילות של JSON, הוא נהפך לפופולארי ביותר.

בנוסף, ב-Elm ישנן כמה חבילות נושא שמכילות מימושים מתקדמים יותר לעבודה עם JSON, כך שאם אתם מעוניינים להשתמש בו בצורה מתקדמת יותר - תמיד יש אפשרויות לכם.

למידע נוסף על עבודה עם JSON ב-Elm, ניתן לעיין במקורות הקשורים המצוינים במקום לסיום המאמר.

## ראו גם
לצפייה בפרויקט השלם המשמש תיגובה באמצעות JSON, ניתן לבקר ב[משאב זה](https://guide.elm-lang.org/interop/json.html) במדריך הרשמי של Elm.

אם אתם מעוניינים לדעת עוד על JSON ככלי עבודה עם נתונים מעוצבים ברשת, מומלץ לעיין באתר [JSON.org](https://www.json.org/) לקבלת מידע מקיף על הפורמט עצמו.

תודה שקראתם ובהצלחה בעבודה עם JSON ב-Elm!