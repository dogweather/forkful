---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:01.136580-07:00
description: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA \u05D4\u05DD \u05EA\
  \u05D1\u05E0\u05D9\u05D5\u05EA \u05D4\u05DE\u05E9\u05DE\u05E9\u05D5\u05EA \u05DC\
  \u05D4\u05EA\u05D0\u05DE\u05EA \u05E6\u05D9\u05E8\u05D5\u05E4\u05D9 \u05EA\u05D5\
  \u05D5\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA. \u05D1-Elm,\
  \ \u05DB\u05DE\u05D5 \u05D1\u05E9\u05E4\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA\
  , \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\
  \u05DD \u05D1-regex \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05DB\u05DE\u05D5\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05E7\u05DC\u05D8,\u2026"
lastmod: '2024-03-11T00:14:12.629511-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA \u05D4\u05DD \u05EA\
  \u05D1\u05E0\u05D9\u05D5\u05EA \u05D4\u05DE\u05E9\u05DE\u05E9\u05D5\u05EA \u05DC\
  \u05D4\u05EA\u05D0\u05DE\u05EA \u05E6\u05D9\u05E8\u05D5\u05E4\u05D9 \u05EA\u05D5\
  \u05D5\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA. \u05D1-Elm,\
  \ \u05DB\u05DE\u05D5 \u05D1\u05E9\u05E4\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA\
  , \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\
  \u05DD \u05D1-regex \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05DB\u05DE\u05D5\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05E7\u05DC\u05D8,\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
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
