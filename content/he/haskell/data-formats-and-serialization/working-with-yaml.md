---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:53.428991-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05DC-Haskell \u05D0\u05D9\u05DF \u05EA\u05DE\
  \u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC\u05E2\u05D9\u05D1\
  \u05D5\u05D3 YAML, \u05D0\u05DA \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\
  \u05DE\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\
  \u05D9\u05E9\u05D9 \u05DB\u05D2\u05D5\u05DF `yaml` \u05D5-`aeson` \u05DC\u05E0\u05D9\
  \u05EA\u05D5\u05D7 \u05D5\u05D9\u05E6\u05D9\u05E8\u05EA \u05E0\u05EA\u05D5\u05E0\
  \u05D9 YAML. \u05D4\u05E0\u05D4 \u05D0\u05D9\u05DA \u05D0\u05EA\u05DD \u05D9\u05DB\
  \u05D5\u05DC\u05D9\u05DD \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC."
lastmod: '2024-04-05T21:53:40.607857-06:00'
model: gpt-4-0125-preview
summary: "\u05DC-Haskell \u05D0\u05D9\u05DF \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\
  \u05D1\u05E0\u05D9\u05EA \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3 YAML, \u05D0\u05DA\
  \ \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\
  \u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DB\u05D2\
  \u05D5\u05DF `yaml` \u05D5-`aeson` \u05DC\u05E0\u05D9\u05EA\u05D5\u05D7 \u05D5\u05D9\
  \u05E6\u05D9\u05E8\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9 YAML."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## איך ל:
ל-Haskell אין תמיכה מובנית לעיבוד YAML, אך ניתן להשתמש בספריות צד שלישי כגון `yaml` ו-`aeson` לניתוח ויצירת נתוני YAML. הנה איך אתם יכולים להתחיל:

### קריאת YAML
ראשית, הוסיפו את החבילה `yaml` לתלות של הפרויקט שלכם. לאחר מכן, תוכלו להשתמש בדוגמה הבאה לניתוח מסמך YAML פשוט:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- נתוני YAML לדוגמה
yamlData :: ByteString
yamlData = "
name: John Doe
age: 30
"

-- הגדרת מבנה נתונים שמתאים למסמך YAML
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show)

instance FromYAML Person where
  parseYAML = withMap "Person" $ \m -> Person
    <$> m .: "name"
    <*> m .: "age"

main :: IO ()
main = do
  let parsed = decode1 yamlData :: Either (Pos,String) Person
  case parsed of
    Left err -> putStrLn $ "שגיאה בניתוח YAML: " ++ show err
    Right person -> print person
```
פלט לדוגמה עבור הקוד לעיל יכול להיראות כך:
```
Person {name = "John Doe", age = 30}
```

### כתיבת YAML
כדי ליצור YAML ממבני נתונים של Haskell, ניתן להשתמש ביכולות הקידוד של החבילה `yaml` כפי שמוצג למטה:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- שימוש במבנה הנתונים Person מהדוגמה הקודמת

person :: Person
person = Person "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 person
  putStrLn $ unpack yamlData
```
פלט של תוכנית זו יהיה מחרוזת בפורמט YAML:
```
name: Jane Doe
age: 25
```

דוגמאות אלו אמורות לשמש נקודת התחלה לעבודה עם YAML ב-Haskell. בהתאם לצרכים שלכם, כדאי יהיה לחקור תכונות ואפשרויות מתקדמות יותר המוצעות על ידי ספריות אלה.
