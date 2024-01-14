---
title:    "Elm: קריאת קובץ טקסט"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## למה

כתיבת קוד היא קשה, ואם אתה משתמש בשפת תכנות מורכבת כמו Elm, ניתן להתקשר בקלות כי אתה מתפנק ואין לך סיבה לענות על שאלות כמו "למה כדאי לקרוא קובץ טקסט?" אבל, ישנם מצבים רבים בהם יש לנו את הצורך לקרוא קבצי טקסט, לדוגמה בעת עבודה עם קבצי קוד או בעת עבודה עם מידע מאורגן בקבצים מסוג CSV או JSON. לכן, כדאי ללמוד כיצד לקרוא ולעבוד עם קבצי טקסט בכדי להיות מוכנים לכל מצב.

## כיצד לבצע זאת

האתגר הכי גדול בקריאת קבצי טקסט הוא לאתר את הקובץ ולקרוא אותו במבנה הנכון. לכן, נראה דוגמאות קוד בשפת Elm המדגימות כיצד לקרוא קבצי טקסט ולהדפיס את התוכן שלהם:

```Elm
import File
import String

-- קריאת קובץ והדפסת התוכן שלו
printTextFile : String -> Cmd msg
printTextFile path =
    File.read path
        |> Task.perform (always ()) (String.print)

-- קריאת קובץ CSV והדפסת הנתונים בצורה מאורגנת
import Csv

printCsvFile : String -> Cmd msg
printCsvFile path =
    File.read path
        |> Task.andThen (Csv.parse Csv.row)
        |> Task.perform (always ()) (List.map Tuple.first >> List.map String.fromInt >> String.join "," >> Debug.log "CSV Values")

-- קריאת קובץ JSON והדפסת העץ של המידע שבו
import Json.Decode as Decode

jsonDecoder : Decode.Decoder String
jsonDecoder =
    Decode.string

printJsonFile : String -> Cmd msg
printJsonFile path =
    File.read path
        |> Task.andThen (Decode.decodeString jsonDecoder)
        |> Task.perform (always ()) (String.fromList >> Debug.log "JSON Tree")
```

כעת ניתן להריץ את הפונקציות הנ"ל עם מסלול של קובץ טקסט תקין, לדוגמה "/assets/myFile.txt" ולראות את התוצאות המדויקות בלוג הדיבאג של הפונקציות.

## מעמיקים יותר

קריאת קבצי טקסט היא ר