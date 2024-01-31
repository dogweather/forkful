---
title:                "עבודה עם YAML"
date:                  2024-01-19
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט נתונים קל לקריאה המשמש לתצורה ולהעברת מידע. תכנתים משתמשים בו בכדי לשפר את האינטראקטיביות עם משתמשים, להגדיר תצורה וכן לסדר ולנהל נתונים בקוד.

## איך לעשות:
לעבודה עם YAML ב-Haskell, נפוץ להשתמש בחבילת `yaml`. ראשית, התקין את החבילה על ידי הוספת `yaml` ל-`build-depends` בקובץ `your_project.cabal`, או על ידי הרצת `cabal install yaml`.

```Haskell
import Data.Yaml
import qualified Data.ByteString.Char8 as BS

-- דוגמה: קריאה של YAML מתוך מחרוזת
main :: IO ()
main = do
    let yamlData = BS.pack "name: Yossi\nage: 30\n"
    let parsedData = decodeThrow yamlData :: IO (Maybe Object)

    -- הדפס את התוצאות
    parsedData >>= print
```

תוצאה צפויה:
```
Just (Object (fromList [("name",String "Yossi"),("age",Number 30.0)]))
```

## עמק השווה:
YAML (יש לקרוא “יאמל”), שפירושו "YAML Ain't Markup Language", הוא פורמט נתונים שהומצא בתחילת שנות ה-2000 כאלטרנטיבה הנגישה יותר ל-XML. חלופות נפוצות כוללות JSON וXML, אבל YAML מתבלט בקריאות הטבעית שלו. בהקשר של המימוש ב-Haskell, קיימות חבילות נוספות כמו `HsYaml` שכדאי לבחון.

## לראות גם:
- [The official YAML website](https://yaml.org)
- [YAML package on Hackage](https://hackage.haskell.org/package/yaml)
- [Aeson package for JSON in Haskell](https://hackage.haskell.org/package/aeson)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
