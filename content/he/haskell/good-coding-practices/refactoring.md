---
title:                "רפקטורינג"
aliases: - /he/haskell/refactoring.md
date:                  2024-01-26T01:39:04.467488-07:00
model:                 gpt-4-0125-preview
simple_title:         "רפקטורינג"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/refactoring.md"
---

{{< edit_this_page >}}

## מה ולמה?
ריפקטורינג הוא התהליך שבו מתאמים את הקוד שלך מבלי לשנות את התנהגותו החיצונית. זה כל הקשור לניקוי וארגון הפעולה שלך כדי להפוך את הקוד לקריא יותר, ניתן לתחזוק ולהרחבה. זה יכול גם לעזור לדכא באגים ולשפר את הביצועים.

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
