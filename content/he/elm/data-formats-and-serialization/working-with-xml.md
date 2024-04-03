---
date: 2024-01-26 04:30:55.241264-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Elm, \u05D0\
  \u05EA\u05D4 \u05E2\u05D5\u05E1\u05E7 \u05D1-XML \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\
  \u05EA \u05D7\u05D1\u05D9\u05DC\u05EA `elm/xml`. \u05D4\u05E0\u05D4 \u05DE\u05D1\
  \u05D8 \u05DE\u05D4\u05D9\u05E8 \u05E2\u05DC \u05E4\u05D9\u05E8\u05D5\u05E1 \u05E7\
  \u05D8\u05E2 XML."
lastmod: '2024-03-13T22:44:39.242512-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Elm, \u05D0\u05EA\u05D4 \u05E2\u05D5\u05E1\u05E7 \u05D1-XML \u05D1\
  \u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D7\u05D1\u05D9\u05DC\u05EA `elm/xml`."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
weight: 40
---

## איך לעשות:
ב-Elm, אתה עוסק ב-XML באמצעות חבילת `elm/xml`. הנה מבט מהיר על פירוס קטע XML:

```Elm
import Xml.Decode exposing (..)
import Xml.Decode.Pipeline exposing (..)

xmlString = """
<book id="123">
    <title>Elm in Action</title>
    <author>Robin Heggelund Hansen</author>
</book>
"""

type alias Book =
    { id : String
    , title : String
    , author : String
    }

bookDecoder : Decoder Book
bookDecoder =
    decode Book
        |> required "id" (attribute "id")
        |> required "title" (child "title" (content text))
        |> required "author" (child "author" (content text))

case Xml.Decode.fromString bookDecoder xmlString of
    Ok book ->
        -- עשה משהו עם הספר שהתפרס כאן
        Debug.toString book

    Err error ->
        -- טפל בשגיאות
        Debug.toString error
```

פלט לדוגמה, בהנחה שאין שגיאות:

```Elm
"{ id = \"123\", title = \"Elm in Action\", author = \"Robin Heggelund Hansen\" }"
```

## צלילה עמוקה
XML (שפת הסימון המורחבת) קיימת מהשנות ה-90 המאוחרות, תקופה בה האינטרנט היה כבד על טקסטים והצורך בדרך מובנית וגמישה להעברת נתונים היה קריטי. בעקבות המסורבלות והמורכבות שלה, XML איבדה קצת מעמדתה לטובת JSON. עם זאת, XML עדיין שכיחה, במיוחד בסביבות ארגוניות או בפרוטוקולים כמו SOAP.

הגישה של Elm ל-XML היא פונקציונלית ובטוחה מבחינת טיפוסים. שימוש בחבילת `elm/xml` מתבסס על פילוסופיה של Elm של מפורשות ואמינות. כאשר מדובר בפירוס, החבילה מספקת מבחר של מפענחים שאתה מרכיב יחד כדי לטפל במבנה ה-XML.

בהשוואה לאלטרנטיבות כמו DOMParser של JavaScript או ElementTree של Python, שיטת Elm יכולה להיראות יותר מסורבלת אך היא מבטיחה בטיחות. אין חריגות בזמן ריצה עבור שדות חסרים או אי התאמות טיפוס; אם משהו לא בסדר, אתה מקבל שגיאה בזמן קומפילציה.

פונקציות הפירוס של `elm/xml` מתמקדות במיפוי צמתי XML לטיפוסים של Elm. אתה בונה מפענחים שמשקפים את צורת הנתונים שלך, ומבטיח שהאפליקציה שלך מטפלת ב-XML בחומרה בדיוק כמו שהיא מטפלת במבני נתונים פנימיים.

יצירת XML היא פחות נפוצה ב-Elm אך ניתן להשיגה באמצעות חבילת התאימות `Xml.Encode`.

## ראה גם
- המדריך של Elm ל-JSON שגם חל על גישת XML: [https://guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
- תקן XML מאת W3C להבנה עמוקה יותר של XML עצמה: [https://www.w3.org/XML/](https://www.w3.org/XML/)
