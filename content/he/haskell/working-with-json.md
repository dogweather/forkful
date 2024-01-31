---
title:                "עבודה עם JSON"
date:                  2024-01-19
simple_title:         "עבודה עם JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?

JSON הוא פורמט של נתונים שמיועד לשמירת והעברת מידע באופן קליל וקריא. תוכניתנים משתמשים בו כי הוא נפוץ, קל לתפעול ומתאם טוב לשיתוף נתונים בין שרתים ולקוחות.

## איך לעשות:

בואו נשתמש בספריית `aeson` לעבוד עם JSON ב-Haskell.

התקינו את `aeson`:

```bash
cabal update
cabal install aeson
```

קוד לעיבוד JSON פשוט:

```Haskell
import Data.Aeson
import Data.ByteString.Lazy as B
import Data.Text
import Control.Monad

-- דוגמה למבנה JSON
jsonInput :: ByteString
jsonInput = "{\"name\":\"John\", \"age\":30}"

-- דוגמה להמרה של JSON למילון ב-Haskell
decodeJSON :: ByteString -> Maybe Object
decodeJSON = decode

main = do
  let maybeResult = decodeJSON jsonInput
  case maybeResult of
    Just result -> print result
    Nothing -> putStrLn "פרסור ה-JSON נכשל"
```

פלט לדוגמא:

```
fromList [("name", String "John"), ("age", Number 30.0)]
```

## טבילה עמוקה

`aeson` נכנס לשימוש ב-2011 והפך לסטנדרט בעבודה עם JSON ב-Haskell. קיימות אלטרנטיבות כמו `json` ו-`yaml`, אבל `aeson` הוא הפופולרי ביותר. הספרייה מתמך בהמרות אוטומטיות למודלים המיוצגים דרך סוגים אלגבריים, עם ממשק פשוט להרחבה והתאמה אישית.

## ראה גם

- [מדריך רשמי לספריית aeson](https://hackage.haskell.org/package/aeson)
- [תיעוד JSON ב-MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)
- [מאמר על סוגים אלגבריים ב-Haskell](https://wiki.haskell.org/Algebraic_data_type)
