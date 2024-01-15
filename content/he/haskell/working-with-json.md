---
title:                "עבודה עם JSON"
html_title:           "Haskell: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## למה
אנשים יכולים להתעסק בעבודה עם JSON כדי לטפל בנתונים מורכבים כמו מידע מקימת API או קבצי הגדרת התצורה.

## איך לבצע זאת
אתם יכולים לעקוב אחר כמה פעולות פשוטות כדי לשנות את ייצוג הנתונים שלכם לתצורת JSON בHaskell. הנה דוגמה לקוד המראה איך לממש קבוצת נתונים כתצורת JSON:

```Haskell
-- כתיבת מודולים מתאימים
import Data.Aeson
import Data.ByteString.Char8 as C8
import Data.Text.Encoding as E

-- יצירת נתוני דוגמה
data User = User { name :: String
                 , age :: Int
                 , email :: String
                 } deriving (Show, Generic)

-- נמירה לתצורת JSON
instance ToJSON User

-- הדפסת התצורה שלנו לתצורת JSON
main = do
    let user = User "John" 30 "john@example.com"
    let json = encode user
    -- ממירים את התצורה לstring סודי כדי להדפיס
    C8.putStrLn $ E.decodeUtf8 json
```

הפלט:

```Output
{"age":30,"email":"john@example.com","name":"John"}
```

## הצגה מעמיקה
כאשר אתם מתחילים לעבוד עם JSON בHaskell, כדאי ללמוד מה הם ספריות המכשירים הטובות ביותר כדי לעבוד עם נתונים מורכבים. כמו כן, משתלב עם גוף רב שונות תומך הנתונים מידע אודות תעדוף, תקינה ועוד.

## ראו גם:
- [המוכנה לשימוש ספריות JSON](https://hackage.haskell.org/package/aeson)
- [מדריך איך להשתמש כמו לרכבת handling JSON עם Haskell](https://serokell.io/blog/handling-json-with-haskell)
- [ממחשב תרגילים כדי לעבוד עם נתוני האיטרנט מקימת API או סמנטכיוס מערכת](https://github.com/nikita-volkov/semantic-refacto