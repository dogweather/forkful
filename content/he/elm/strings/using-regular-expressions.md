---
title:                "שימוש בביטויים רגולריים"
aliases: - /he/elm/using-regular-expressions.md
date:                  2024-02-03T19:17:01.136580-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בביטויים רגולריים"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
ביטויים רגולריים (regex) בתכנות הם תבניות המשמשות להתאמת צירופי תווים במחרוזות. ב-Elm, כמו בשפות אחרות, מתכנתים משתמשים ב-regex למשימות כמו אימות קלט, חיפוש, והחלפת טקסט בתוך מחרוזות בשל גמישותם ויעילותם.

## איך ל:
ב-Elm אין פונקציות regex מובנות בספרייה המרכזית שלה, מה שדורש שימוש בספריות צד שלישי לפעולות אלו. אחת מהאפשרויות הפופולריות לעבודה עם regex היא `elm/regex`. ניתן להוסיף אותה לפרויקט שלך באמצעות `elm install elm/regex`.

הנה איך אפשר להשתמש ב-`elm/regex` למשימות נפוצות כמה:

### 1. התאמה לתבנית
כדי לבדוק אם מחרוזת תואמת לתבנית, ניתן להשתמש ב-`Regex.contains`.

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- דוגמה לשימוש:
isAlphanumeric "Elm2023"     -- פלט: True
isAlphanumeric "Elm 2023!"   -- פלט: False
```

### 2. מציאת כל ההתאמות
כדי למצוא את כל המופעים של תבנית בתוך מחרוזת, ניתן להשתמש ב-`Regex.find`.

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- דוגמה לשימוש:
getWords "Elm is fun!"  -- פלט: ["Elm", "is", "fun"]
```

### 3. החלפת טקסט
כדי להחליף חלקים ממחרוזת שתואמים לתבנית, משתמשים ב-`Regex.replace`.

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- דוגמה לשימוש:
replaceElmWithHaskell "Learning Elm is fun!"  
-- פלט: "Learning Haskell is fun!"
```

בדוגמאות אלו, משתמשים ב-`Regex.fromString` כדי לקומפל regex pattern, שבו `\b` מתאים לגבולות מילים, ו-`\w` מתאים לכל תו של מילה. תמיד נהוג לטפל בתוצאת `Maybe` של `Regex.fromString` כדי להגן על נגד תבניות regex לא תקינות, בדרך כלל באמצעות `Maybe.withDefault`.
