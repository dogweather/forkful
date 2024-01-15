---
title:                "לעבוד עם Yaml"
html_title:           "Haskell: לעבוד עם Yaml"
simple_title:         "לעבוד עם Yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

כתיבת קוד באמצעות YAML בשפת הפונקציונליות Haskell יכולה להיות דרך נוחה וקלה להגדיר תצורות שונות. הוא מציע תחביר קריא ומתקדם שיאפשר לך לעבוד עם מגוון רחב של נתונים בחסות שפת מתכנת ההתגוננות העוצמתית הזו.

## איך לעשות זאת

קוד ה-Haskell הבא מדגים כיצד ניתן ליצור מבני YAML באמצעות הספרייה הפונקציונלית "yaml". ניתן לראות בתוך הקוד דוגמאות של הגדרות רבות ומגוונות של מבני YAML ואת הפלט המתקבל.

```Haskell
import Data.Yaml

data Person = Person
  { name :: String
  , age :: Int
  , location :: String
  }

person1 :: Person
person1 = Person { name = "John", age = 30, location = "New York" }

person2 :: Person
person2 = Person { name = "Sarah", age = 25, location = "Los Angeles" }

main :: IO ()
main = do
  let people = [person1, person2]
  let yaml = encode people
  print people
  putStr "---\n"
  putStr yaml
```

פלט הקוד יהיה הבא:

```yaml
- name: John
  age: 30
  location: New York
- name: Sarah
  age: 25
  location: Los Angeles
```

## התעמולה המעמיקה

פקודה היספה נוספת שיש לכם זמינה כדי לעזור לכם בעבודה עם YAML היא היכן לאחסן את הקבצים שלכם. כדי לגשת לשם, ישנם מספר אפשרויות שאתם יכולים להשתמש בהם עבור תחבירי YAML והם כוללים תבניות הכתיבה העצמתיות והספקית.

## ראו גם

- [Haskell Wiki: YAML](https://wiki.haskell.org/YAML)
- [Hackage: yaml](https://hackage.haskell.org/package/yaml)