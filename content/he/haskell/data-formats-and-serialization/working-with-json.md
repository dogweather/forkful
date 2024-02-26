---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:40.807856-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON (JavaScript Object\
  \ Notation) \u05D1-Haskell \u05DB\u05D5\u05DC\u05DC\u05EA \u05E0\u05D9\u05EA\u05D5\
  \u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9 JSON \u05DC\u05E1\u05D5\u05D2\u05D9\u05DD\
  \ \u05E9\u05DC Haskell \u05D5\u05D4\u05DE\u05E8\u05EA \u05E1\u05D5\u05D2\u05D9\u05DD\
  \ \u05E9\u05DC Haskell \u05D1\u05D7\u05D6\u05E8\u05D4 \u05DC-JSON. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9\u2026"
lastmod: '2024-02-25T18:49:37.687832-07:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON (JavaScript Object Notation)\
  \ \u05D1-Haskell \u05DB\u05D5\u05DC\u05DC\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 \u05E0\
  \u05EA\u05D5\u05E0\u05D9 JSON \u05DC\u05E1\u05D5\u05D2\u05D9\u05DD \u05E9\u05DC\
  \ Haskell \u05D5\u05D4\u05DE\u05E8\u05EA \u05E1\u05D5\u05D2\u05D9\u05DD \u05E9\u05DC\
  \ Haskell \u05D1\u05D7\u05D6\u05E8\u05D4 \u05DC-JSON. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם JSON (JavaScript Object Notation) ב-Haskell כוללת ניתוח נתוני JSON לסוגים של Haskell והמרת סוגים של Haskell בחזרה ל-JSON. מתכנתים עושים זאת כדי לאפשר ליישומי ה-Haskell שלהם להחליף נתונים עם שירותי אינטרנט או API-ים בצורה חלקה, פרקטיקה נפוצה בפיתוח תוכנה מודרני להחלפת נתונים בין פלטפורמות.

## איך לעשות:
ל-Haskell אין תמיכה מובנית ל-JSON כמו ב-JavaScript, אבל עם עזרת ספריות צד שלישי כמו **Aeson**, הטיפול ב-JSON הופך לפשוט. Aeson מספקת פונקציות ברמה גבוהה ונמוכה גם לקידוד (המרת ערכי Haskell ל-JSON) וגם לפענוח (ניתוח JSON לערכי Haskell).

### התקנת Aeson
ראשית, הוסף את Aeson לתלות של הפרויקט שלך על ידי עדכון הקובץ `.cabal` שלך או שימוש ב-Stack או Cabal ישירות:

```shell
cabal update && cabal install aeson
```
או, אם אתה משתמש ב-Stack:
```shell
stack install aeson
```

### ניתוח JSON
בואו נתחיל עם דוגמא בסיסית של פענוח נתוני JSON לסוג של Haskell. נניח שיש לנו את ה-JJSON הבא המייצג אדם:

```json
{
  "name": "John Doe",
  "age": 30
}
```

ראשית, הגדר סוג נתונים מתאים ב-Haskell והפוך אותו למופע של `FromJSON`:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

instance FromJSON Person

-- פונקציה לפענוח JSON מקובץ
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
שימוש:
בהנחה ו-`person.json` מכיל את נתוני ה-JSON שהוצגו למעלה, הפעל:
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
פלט דוגמה:
```haskell
Just (Person {name = "John Doe", age = 30})
```

### קידוד ערכי Haskell כ-JSON
כדי להמיר ערך של Haskell בחזרה ל-JSON, אתה צריך להפוך את הטיפוס שלך למופע של `ToJSON` ואז להשתמש ב-`encode`.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- בהנחה שהסוג Person מהדוגמא הקודמת

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
פלט דוגמה:
```json
{"name":"Jane Doe","age":32}
```

הדוגמאות האלו מדגימות את היסודות של עבודה עם JSON ב-Haskell באמצעות Aeson. זכרו, Aeson מציעה הרבה יותר, כולל חוקי ניתוח מותאמים אישית, עבודה עם JSON מורכב ומקונן ועוד הרבה, המתאימים לצרכים ולתרחישים שונים.
