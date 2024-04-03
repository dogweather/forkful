---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:23.516121-07:00
description: "\u05DB\u05D9\u05E6\u05D3 \u05DC: \u05D1Haskell, \u05E4\u05D5\u05E0\u05E7\
  \u05E6\u05D9\u05D5\u05EA regex \u05D0\u05D9\u05E0\u05DF \u05D7\u05DC\u05E7 \u05DE\
  \u05D4\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\
  \u05D9\u05EA, \u05DE\u05D4 \u05E9\u05DE\u05D7\u05D9\u05D9\u05D1 \u05E9\u05D9\u05DE\
  \u05D5\u05E9 \u05D1\u05D7\u05D1\u05D9\u05DC\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\
  \u05D9\u05E9\u05D9 \u05DB\u05DE\u05D5 `regex-base` \u05D9\u05D7\u05D3 \u05E2\u05DD\
  \ \u05EA\u05D5\u05DB\u05E0\u05EA backend \u05DE\u05EA\u05D0\u05D9\u05DE\u05D4 \u05DB\
  \u05DE\u05D5 `regex-\u2026"
lastmod: '2024-03-13T22:44:39.396627-06:00'
model: gpt-4-0125-preview
summary: "\u05D1Haskell, \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA regex \u05D0\
  \u05D9\u05E0\u05DF \u05D7\u05DC\u05E7 \u05DE\u05D4\u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA, \u05DE\u05D4 \u05E9\
  \u05DE\u05D7\u05D9\u05D9\u05D1 \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D7\u05D1\
  \u05D9\u05DC\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DB\u05DE\
  \u05D5 `regex-base` \u05D9\u05D7\u05D3 \u05E2\u05DD \u05EA\u05D5\u05DB\u05E0\u05EA\
  \ backend \u05DE\u05EA\u05D0\u05D9\u05DE\u05D4 \u05DB\u05DE\u05D5 `regex-posix`\
  \ (\u05DC\u05EA\u05DE\u05D9\u05DB\u05D4 \u05D1regex \u05E9\u05DC POSIX), `regex-pcre`\
  \ (\u05DC\u05EA\u05DE\u05D9\u05DB\u05D4 \u05D1regex \u05EA\u05D5\u05D0\u05DD \u05DC\
  Perl), \u05D5\u05DB\u05D3\u05D5\u05DE\u05D4."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
weight: 11
---

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
