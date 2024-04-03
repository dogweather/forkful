---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:46.605846-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05EA\
  \ \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8 (stderr) \u05D1\u05D4\u05D0\u05E1\u05E7\u05DC\
  \ \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05DB\u05E0\u05D9\u05D5\u05EA\
  \ \u05DC\u05D4\u05D1\u05D3\u05D9\u05DC \u05D1\u05D9\u05DF \u05D4\u05EA\u05D5\u05E6\
  \u05D0\u05D5\u05EA \u05D4\u05E8\u05D2\u05D9\u05DC\u05D5\u05EA \u05DC\u05D1\u05D9\
  \u05DF \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05D4\u05E9\u05D2\u05D9\u05D0\u05D4\
  . \u05D6\u05D4 \u05E7\u05E8\u05D9\u05D8\u05D9 \u05DC\u05E6\u05D5\u05E8\u05DA \u05D0\
  \u05D9\u05EA\u05D5\u05EA \u05D1\u05E2\u05D9\u05D5\u05EA \u05D5\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA, \u05DC\u05DC\u05D0 \u05E9\u05D9\
  \u05D1\u05D5\u05E9 \u05D4\u05E4\u05DC\u05D8\u2026"
lastmod: '2024-03-13T22:44:39.439875-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05EA \u05E1\
  \u05D8\u05E0\u05D3\u05E8\u05D8 (stderr) \u05D1\u05D4\u05D0\u05E1\u05E7\u05DC \u05DE\
  \u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05DB\u05E0\u05D9\u05D5\u05EA \u05DC\
  \u05D4\u05D1\u05D3\u05D9\u05DC \u05D1\u05D9\u05DF \u05D4\u05EA\u05D5\u05E6\u05D0\
  \u05D5\u05EA \u05D4\u05E8\u05D2\u05D9\u05DC\u05D5\u05EA \u05DC\u05D1\u05D9\u05DF\
  \ \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05D4\u05E9\u05D2\u05D9\u05D0\u05D4."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
weight: 25
---

## איך לעשות:
בהאסקל, כתיבה ל-stderr היא פשוטה באמצעות מודול `System.IO` של הספרייה הבסיסית. להלן דוגמה בסיסית להדגמה:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "This is an error message."
```

פלט התכנית הזו ל-stderr יהיה:

```
This is an error message.
```

אם אתה עובד ביישום מורכב יותר, או שאתה זקוק לשליטה טובה יותר על רישום הלוגים (כולל השגיאות), ייתכן שתבחר בספרייה מצד שלישי. בחירה פופולרית היא `monad-logger` שמשתלבת עם סגנון התכנות של `mtl` בהאסקל. הנה קטע קטן באמצעות `monad-logger`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "This is an error message using monad-logger."
```

כאשר מריצים, גרסת ה-`monad-logger` מוציאה באופן דומה הודעת שגיאה, אך היא מצוידת ביותר הקשר כמו חותמות זמן או רמות לוג, בהתאם לתצורה:

```
[Error] This is an error message using monad-logger.
```

שני השיטות משרתות את המטרה של כתיבה ל-stderr, כאשר הבחירה תלויה ברובה במורכבות ובצרכים של היישום שלך.
