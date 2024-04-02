---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:53.428991-07:00
description: "YAML, \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\u05EA \u05E9\
  \u05DC \"YAML Ain't Markup Language\", \u05D4\u05D5\u05D0 \u05EA\u05E7\u05DF \u05E1\
  \u05D9\u05D3\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D9\u05D3\u05D9\
  \u05D3\u05D5\u05EA\u05D9 \u05DC\u05D0\u05D3\u05DD, \u05E9\u05E0\u05D9\u05EA\u05DF\
  \ \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D5 \u05E2\u05D1\u05D5\u05E8 \u05DB\
  \u05DC \u05E9\u05E4\u05D5\u05EA \u05D4\u05EA\u05DB\u05E0\u05D5\u05EA. \u05EA\u05DB\
  \u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\
  \u05E8\u05D5\u05D1\u05D5\u05EA \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.446295-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\u05EA \u05E9\u05DC\
  \ \"YAML Ain't Markup Language\", \u05D4\u05D5\u05D0 \u05EA\u05E7\u05DF \u05E1\u05D9\
  \u05D3\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D9\u05D3\u05D9\u05D3\
  \u05D5\u05EA\u05D9 \u05DC\u05D0\u05D3\u05DD, \u05E9\u05E0\u05D9\u05EA\u05DF \u05DC\
  \u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D5 \u05E2\u05D1\u05D5\u05E8 \u05DB\u05DC\
  \ \u05E9\u05E4\u05D5\u05EA \u05D4\u05EA\u05DB\u05E0\u05D5\u05EA. \u05EA\u05DB\u05E0\
  \u05D9\u05EA\u05E0\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\
  \u05D5\u05D1\u05D5\u05EA \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## מה ולמה?

YAML, ראשי תיבות של "YAML Ain't Markup Language", הוא תקן סידור נתונים ידידותי לאדם, שניתן להשתמש בו עבור כל שפות התכנות. תכניתנים לעיתים קרובות משתמשים ב-YAML בקובצי תצורה ובחליפין נתונים בין שפות בשל הקריאות ומבנה הפשוט שלו.

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
