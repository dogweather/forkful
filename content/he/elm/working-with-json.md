---
title:                "Elm: לעבוד עם json"
simple_title:         "לעבוד עם json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-json.md"
---

{{< edit_this_page >}}

## למה

JSON היא שפת תוכנות קלה וממוקדת שתפיק את משחק הדגל שלכם במתחמי פיתוח היום בזמן! כתוצאה משפע המידע העצום ברשתות, מקצוע הפיתוח מחייב תקשורת יעילה ותמיכה בפורמט שיאומות מעמידות בפניו כפונקציות! עם יישומוני מטרואילנד אינטרנט אינטרנט לב־זמַן שלהם נקבל הצגה למידע על־זה שכדאי!

## איך לעשות זאת

```Elm
type alias Person =
    { name : String
    , age : Int
    , hobbies : List String
    }

let john =
    { name = "John"
    , age = 25
    , hobbies = [ "reading", "painting", "hiking" ]
    }

personToJson : Person -> String
personToJson person =
    let
        hobbiesJson =
            person.hobbies
            |> List.map (\h -> Json.Encode.string h)
            |> Json.Encode.list
    in
        Json.Encode.object
            [ ( "name", Json.Encode.string person.name )
            , ( "age", Json.Encode.int person.age )
            , ( "hobbies", hobbiesJson )
            ]
```

זהו דוגמא קצרה של כיצד ניתן להמיר מבני נתונים של Elm למבני נתונים של JSON באמצעות הספרייה פנימית של `Json.Encode`. הפונקציה `personToJson` מקבלת אוביקט מסוג `Person` ומחזירה סטרינג שמכיל את הנתונים שלו בתור מבנה JSON. כדי להמיר את התת־רשימה של תחבירי החברות, אנו מאתרים כל תחביר ברשימה וממירים אותו למחרוזת באמצעות הפונקציה `Json.Encode.string`, ואז ממירים את הרשימה למבנה JSON באמצעות `Json.Encode.list`.

כעת, כדי להמיר מבנה JSON למבני נתונים של Elm בשפה אנו משתמשים בספריית פנימית נוספת בשם `Json.Decode`. היא מכילה פונקציות חזרה שהמיר מבני JSON לתת־נתונים של Elm בצורה צינורה ופשוטה.

## צליל עמק

כעת שאנו מבינים כיצד להעביר מבנים של Elm לפורמט של JSON ולהיפך וכיצד לעבוד עם הס