---
date: 2024-01-26 04:21:51.163104-07:00
description: "TOML, \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\u05EA \u05E9\
  \u05DC Tom's Obvious, Minimal Language, \u05D4\u05D9\u05D0 \u05E9\u05E4\u05EA \u05E1\
  \u05E8\u05D9\u05D0\u05DC\u05D9\u05D6\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9 Elm \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E0\u05D4\u05DC\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4 \u05DE\u05DB\u05D9\
  \u05D5\u05D5\u05DF \u05E9\u05D4\u05D9\u05D0 \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DC\
  \u05D0\u05D3\u05DD \u05D5\u05DE\u05DE\u05E4\u05D4\u2026"
lastmod: '2024-03-13T22:44:39.240965-06:00'
model: gpt-4-0125-preview
summary: "TOML, \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\u05EA \u05E9\u05DC\
  \ Tom's Obvious, Minimal Language, \u05D4\u05D9\u05D0 \u05E9\u05E4\u05EA \u05E1\u05E8\
  \u05D9\u05D0\u05DC\u05D9\u05D6\u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9 Elm \u05DE\u05E9\u05EA\u05DE\
  \u05E9\u05D9\u05DD \u05D1\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E0\u05D4\u05DC \u05E0\
  \u05EA\u05D5\u05E0\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4 \u05DE\u05DB\u05D9\u05D5\
  \u05D5\u05DF \u05E9\u05D4\u05D9\u05D0 \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DC\u05D0\
  \u05D3\u05DD \u05D5\u05DE\u05DE\u05E4\u05D4\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
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
