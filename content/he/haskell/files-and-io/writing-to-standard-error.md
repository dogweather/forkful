---
title:                "כתיבה לשגיאה התקנית"
aliases: - /he/haskell/writing-to-standard-error.md
date:                  2024-02-03T19:33:46.605846-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לשגיאת סטנדרט (stderr) בהאסקל מאפשרת לתכניות להבדיל בין התוצאות הרגילות לבין הודעות השגיאה. זה קריטי לצורך איתות בעיות וניפוי שגיאות, ללא שיבוש הפלט הסטנדרטי (stdout) שלעיתים נושא את נתוני התוכנית העיקריים או התוצאה.

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
