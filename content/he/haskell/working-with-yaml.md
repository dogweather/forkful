---
title:                "Haskell: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

# מדוע: למה לעבוד עם YAML?

בניגוד לכתיבת קוד בשפות אחרות, שימוש בפורמט YAML נותן לנו כתיבה נכונה יותר של קוד ולשימוש בו בצורה פשוטה ותקינה. הוא נחשב לעבודה טובה יותר ברגעים שאתה צריך לייצג מידע שונה בקבצי תצוגה ידידותיים לבני אדם.

## כיצד לעשות זאת

ניתן לכתוב קוד YAML בשפת הפונקציונלית הוסקל באמצעות ספריית YAML המובנית. ניתן להתחיל על ידי טעינת הספרייה על ידי שימוש במילולון `import Data.Yaml`. לאחר מכן, ניתן להשתמש בפונקציות כמו `encode` ו- `decode` כדי להמיר בין נתונים וקוד YAML.

בהמשך תוכלו לראות דוגמאות של קוד YAML והתוצאות שלהם בתוך קוד Haskell:

```Haskell
import Data.Yaml

-- ליצור מילון עם מפתחים וערכים בפורמט YAML
myMap = [("key1", 1), ("key2", 2), ("key3", 3)]

-- להמיר מילון לפורמט YAML ולהדפיס אותו
main = putStrLn (encode myMap)

-- פלט: "key1: 1\nkey2: 2\nkey3: 3\n"

-- המיר פורמט YAML למילון ולהדפיס את הערך של אחד המפתחות
decodedMap = decodeEither "key2: 2" :: Either String [(String, Int)]
main = case decodedMap of
    Left err -> putStrLn err
    Right myMap -> print $ lookup "key2" myMap

-- פלט: Just 2
```

## מעמקים

כשאנחנו מתחילים לעבוד עם YAML, יתכן ונתקל בכמה נושאים מעט יותר מורכבים. כמה דוגמאות לכך הן:

- טיפוסי נתונים מורכבים - כיצד לטפל במילונים, רשימות וקבצים מקומיים בפורמט YAML.
- עיבוד ייבוא - כיצד להעביר נתונים מקובץ YAML למבנה נתונים כדי להשתמש בהם בתוך הקוד שלנו.
-