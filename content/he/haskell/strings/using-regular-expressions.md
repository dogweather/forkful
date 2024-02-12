---
title:                "שימוש בביטויים רגולריים"
aliases:
- /he/haskell/using-regular-expressions/
date:                  2024-02-03T19:17:23.516121-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בביטויים רגולריים"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
בתכנות, ביטויים רגולריים הם סדרות של תווים המגדירות דפוס חיפוש, המשמש בדרך כלל לחיפוש ועיבוד מחרוזות. מתכנתי Haskell משתמשים בביטויים רגולריים למשימות החל מהתאמה פשוטה של מחרוזות ועד לעיבוד טקסט מורכב, מנצלים את היעילות והגמישות שלהם בטיפול בנתוני טקסט.

## כיצד ל:
בHaskell, פונקציות regex אינן חלק מהספרייה הסטנדרטית, מה שמחייב שימוש בחבילות צד שלישי כמו `regex-base` יחד עם תוכנת backend מתאימה כמו `regex-posix` (לתמיכה בregex של POSIX), `regex-pcre` (לתמיכה בregex תואם לPerl), וכדומה. הנה איך אתם יכולים להשתמש בחבילות אלו לעבוד עם ביטויים רגולריים.

ראשית, וודאו שהחבילות מותקנות על ידי הוספת `regex-posix` או `regex-pcre` לקובץ `.cabal` של הפרויקט או התקנה דרך cabal ישירות:

```bash
cabal install regex-posix
```
או
```bash
cabal install regex-pcre
```

### שימוש ב-`regex-posix`:

```haskell
import Text.Regex.Posix ((=~))

-- בדיקה אם מחרוזת תואמת לדפוס
isMatch :: String -> String -> Bool
isMatch text pattern = text =~ pattern :: Bool

-- מציאת ההתאמה הראשונה
findFirst :: String -> String -> String
findFirst text pattern = text =~ pattern :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- פלט: True
    print $ findFirst "good morning, good night" "good"
    -- פלט: "good"
```

### שימוש ב-`regex-pcre`:

```haskell
import Text.Regex.PCRE ((=~))

-- מציאת כל ההתאמות
findAll :: String -> String -> [String]
findAll text pattern = text =~ pattern :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- פלט: ["test1","test2","test3"]
```

לכל ספרייה יש את המיוחדות שלה, אך המתודולוגיה הכללית של שימוש ב-`=~` להחלת הregex נשארת עקבית, בין אם מדובר בבדיקת התאמה או בחילוץ תת-מחרוזות. הבחירה בין `regex-posix` ל-`regex-pcre` תלויה ברובה בצרכים של הפרויקט שלכם וביכולות הregex הספציפיות הנדרשות.
