---
title:                "עבודה עם JSON"
date:                  2024-02-03T19:23:40.807856-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
