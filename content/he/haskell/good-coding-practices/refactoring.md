---
date: 2024-01-26 01:39:04.467488-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E0\u05E0\u05D9\
  \u05D7 \u05E9\u05D9\u05E9 \u05DC\u05DA \u05D7\u05EA\u05D9\u05DB\u05EA \u05E7\u05D5\
  \u05D3 \u05E9\u05DC Haskell \u05E9\u05DE\u05D7\u05D6\u05D5\u05E8 \u05E2\u05DC \u05E2\
  \u05E6\u05DE\u05D4 \u05D9\u05D5\u05EA\u05E8 \u05DE\u05D4\u05E9\u05D9\u05E8 \u05D4\
  \u05D0\u05D4\u05D5\u05D1 \u05E2\u05DC\u05D9\u05DA. \u05D4\u05E0\u05D4 \u05DE\u05D1\
  \u05D8 \u05DE\u05D4\u05D9\u05E8 \u05E2\u05DC \u05D0\u05D9\u05DA \u05EA\u05D5\u05DB\
  \u05DC \u05DC\u05E2\u05E9\u05D5\u05EA \u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\
  \u05D9\u05E0\u05D2 \u05DC\u05D6\u05D4 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA. \u05DC\u05E4\u05E0\u05D9\u2026"
lastmod: '2024-03-13T22:44:39.427146-06:00'
model: gpt-4-0125-preview
summary: "\u05E0\u05E0\u05D9\u05D7 \u05E9\u05D9\u05E9 \u05DC\u05DA \u05D7\u05EA\u05D9\
  \u05DB\u05EA \u05E7\u05D5\u05D3 \u05E9\u05DC Haskell \u05E9\u05DE\u05D7\u05D6\u05D5\
  \u05E8 \u05E2\u05DC \u05E2\u05E6\u05DE\u05D4 \u05D9\u05D5\u05EA\u05E8 \u05DE\u05D4\
  \u05E9\u05D9\u05E8 \u05D4\u05D0\u05D4\u05D5\u05D1 \u05E2\u05DC\u05D9\u05DA."
title: "\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2"
weight: 19
---

## איך לעשות:
נניח שיש לך חתיכת קוד של Haskell שמחזור על עצמה יותר מהשיר האהוב עליך. הנה מבט מהיר על איך תוכל לעשות ריפקטורינג לזה באמצעות פונקציות.

לפני ריפקטורינג:

```haskell
printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  putStrLn $ "Customer: " ++ customer
  putStrLn $ "Total: " ++ show total
  putStrLn $ "Item: " ++ item
```

אחרי קצת ריפקטורינג:

```haskell
printDetail :: String -> String -> IO ()
printDetail label value = putStrLn $ label ++ ": " ++ value

printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  printDetail "Customer" customer
  printDetail "Total" (show total)
  printDetail "Item" item

-- פלט לדוגמה:
-- לקוח: אליס
-- סה"כ: $42.00
-- פריט: מדריך לתכנות ב-Haskell
```

כפי שאתה יכול לראות, על ידי הוצאת התבנית המשותפת לפונקציה נפרדת `printDetail`, אנחנו מונעים חזרה על עצמנו ועושים את הפונקציה `printInvoice` נקייה וקלה יותר לניהול.

## צלילה עמוקה
כאשר Haskell הגיע לזירה בסוף שנות ה-80, היה ברור שהפרדיגמה הפונקציונלית יכולה להביא אוויר צח לתיקי הקוד. ממשיכים משם, וריפקטורינג ב-Haskell הוא אלגנטי במיוחד בזכות העובדה שפונקציות הן אזרחיות ראשונות ובזכות המערכת הטיפוסית הסטטית והחזקה שלו. אתה מבצע ריפקטורינג ללא פחד שתשבור את האפליקציה שלך, מאחר והמהדר מגן עליך.

חלופות לריפקטורינג ידני עשויות לכלול שימוש בכלים אוטומטיים, אך הטבע הפונקציונלי ובטחון הטיפוס של Haskell לפעמים יכולים לגרום לזה להיות נפוץ פחות מאשר בשפות אחרות. מבחינת ביצוע, חשוב לנצל את התכונות של Haskell כגון פונקציות מסדר גבוה, טהרות, ובלתי-שגיאות להפוך את הריפקטורינג לחלק.

עיבודים כמו "הוצאת פונקציה", כפי שהוצג כאן, הם שכיחים, אך ניתן גם לבצע "הכנסת פונקציה", "שינוי שם של משתנה", ו"שינוי חתימת פונקציה" בביטחון, הודות למערכת הטיפוס. ההסקה הטיפוסית החזקה של Haskell לעתים יכולה לתפוס שגיאות שיכולות להימנע בשפות אחרות.

## ראה גם
לעיון נוסף בריפקטורינג ב-Haskell, קרא את הספר "Refactoring: Improving the Design of Existing Code" מאת מרטין פאולר, שבו המושגים תקפים לכלל. בדוק את כלי ה-hlint לרמזים אוטומטיים על שיפור הקוד שלך ב-Haskell. כמו כן, בקר בוויקי של Haskell (https://wiki.haskell.org/Refactoring) לתובנות מהקהילה וקריאה נוספת.
