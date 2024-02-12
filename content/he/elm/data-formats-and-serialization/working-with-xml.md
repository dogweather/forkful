---
title:                "עבודה עם XML"
aliases:
- /he/elm/working-with-xml/
date:                  2024-01-26T04:30:55.241264-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם XML כוללת פירוס, המרה, ויצירת מסמכי XML ב-Elm. מטרת התהליך היא כדי לתקשר עם שירותי אינטרנט רבים ומערכות מורשת שמשתמשות ב-XML כפורמט הנתונים שלהן.

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
