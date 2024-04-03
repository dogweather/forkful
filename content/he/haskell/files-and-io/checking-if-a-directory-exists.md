---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:47.306647-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Haskell, \u05D3\u05E8\
  \u05DA \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05D1\u05E1\u05D9\u05E1 \u05E9\
  \u05DC\u05D4, \u05DE\u05E6\u05D9\u05E2\u05D4 \u05D3\u05E8\u05DB\u05D9\u05DD \u05D9\
  \u05E9\u05E8\u05D5\u05EA \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\u05DD \u05EA\u05D9\
  \u05E7\u05D9\u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA, \u05D1\u05E2\u05D9\u05E7\
  \u05E8 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05DE\u05D5\u05D3\u05D5\
  \u05DC `System.Directory`. \u05D1\u05D5\u05D0\u05D5 \u05E0\u05E2\u05D9\u05D9\u05DF\
  \ \u05D1\u05D3\u05D5\u05D2\u05DE\u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA."
lastmod: '2024-03-13T22:44:39.436614-06:00'
model: gpt-4-0125-preview
summary: "Haskell, \u05D3\u05E8\u05DA \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05D1\
  \u05E1\u05D9\u05E1 \u05E9\u05DC\u05D4, \u05DE\u05E6\u05D9\u05E2\u05D4 \u05D3\u05E8\
  \u05DB\u05D9\u05DD \u05D9\u05E9\u05E8\u05D5\u05EA \u05DC\u05D1\u05D3\u05D5\u05E7\
  \ \u05D0\u05DD \u05EA\u05D9\u05E7\u05D9\u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA\
  , \u05D1\u05E2\u05D9\u05E7\u05E8 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\
  \u05DE\u05D5\u05D3\u05D5\u05DC `System.Directory`."
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
weight: 20
---

## איך לעשות:
Haskell, דרך ספריית הבסיס שלה, מציעה דרכים ישרות לבדוק אם תיקייה קיימת, בעיקר באמצעות המודול `System.Directory`. בואו נעיין בדוגמה בסיסית:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/path/to/your/directory"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "Does the directory exist? " ++ show exists
```

דוגמת פלט, בהתאם לכך אם התיקייה קיימת:

```
Does the directory exist? True
```
או:
```
Does the directory exist? False
```

לתרחישים מורכבים יותר או פונקציונליות נוספת, ייתכן שתשקלו להשתמש בספרייה שלישית פופולרית כמו `filepath` לטיפול ולניפוי נתיבי קבצים באופן יותר מופשט. עם זאת, לצורך פשוט של בדיקה אם תיקייה קיימת, הספריית הבסיס `System.Directory` מספיקה ויעילה.

זכרו, עבודה עם מערכות קבצים יכולה להשתנות בין פלטפורמות, והגישה של Haskell מנסה להפשיט חלק מההבדלים הללו. תמיד בדקו את פעולות הקבצים שלכם על המערכת היעד כדי לוודא התנהגות מצופה.
