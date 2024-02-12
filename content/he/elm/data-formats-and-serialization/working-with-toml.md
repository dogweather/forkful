---
title:                "עבודה עם TOML"
aliases:
- /he/elm/working-with-toml.md
date:                  2024-01-26T04:21:51.163104-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-toml.md"
---

{{< edit_this_page >}}

## מה ולמה?
TOML, ראשי תיבות של Tom's Obvious, Minimal Language, היא שפת סריאליזציה של נתונים. מתכנתי Elm משתמשים בה כדי לנהל נתוני תצורה מכיוון שהיא קריאה לאדם וממפה בצורה נקייה לזוגות מפתח-ערך הדרושים ביישומים.

## איך לעשות:
ל-Elm אין מפענח TOML מובנה, אבל אתה יכול לעבוד עם JavaScript או להשתמש בחבילה מהקהילה. הנה איך תוכל לפענח TOML באמצעות חבילה היפותטית בשם `elm-toml`:

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

לפענוח ערכים ספציפיים:

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

דוגמה לפלט עבור `port` עשויה להיות `Ok 8080` אם הפענוח מצליח.

## עיון מעמיק
TOML נוצרה על ידי תום פרסטון-ורנר, שותף מייסד של GitHub, כשפה פשוטה לקובצי תצורה. היא מתחרה עם YAML ו-JSON; תחביר TOML מכוון לשילוב הטוב ביותר משני העולמות עם מיקוד בכך שיהיה קל לקריאה ולכתיבה עבור בני אדם.

ב-Elm, כדי להתמודד עם TOML, בדרך כלל יש לעבור דרך אינטראופ של JavaScript, שיכול להיות מעט מסורבל. לשמחתנו, הקהילה של Elm משאבית, וקיימות מספר חבילות של צד שלישי. החבילה ההיפותטית `elm-toml` כנראה הייתה משתמשת ב`Port` של Elm כדי לתקשר עם מפענח TOML של JavaScript או לממש את הפענוח ישירות ב-Elm.

המחסום העיקרי ב-Elm הוא שהיא מדויקת סטטית את הכל, כך שיהיה עליך לכתוב מפענחים מותאמים אישית כדי להתמודד עם מבני נתונים שונים בתוך TOML, שיכול להיות מעט ארוך מידי אך מוסיף בטיחות.

## ראה גם
למפרטים ומידע נוסף על TOML עצמה, בקר ב[TOML](https://toml.io).
אם אתה מחפש גישה מעשית לאינטראופ של Elm ו-JavaScript, התחל עם המדריך הרשמי: [Elm Ports](https://guide.elm-lang.org/interop/ports.html).
לחבילות של הקהילה או כדי לתרום, עיין ב[חבילות Elm](https://package.elm-lang.org/).
