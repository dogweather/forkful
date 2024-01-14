---
title:                "Haskell: לעבוד עם json"
simple_title:         "לעבוד עם json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## למה

JSON (JavaScript Object Notation) הוא תבנית נתונים נפוצה בעיצוב אנכי לנתונים. הכתיבה וקריאה של מידע בפורמט זה היא פשוטה וקלה לניתוח ע"י מחשבים ואנשים כאחד. בהינתן שעסקנו עם JSON בפוסטים קודמים, נבין למה כדאי להיכנס לתהליך של עבודה עם JSON בשפת התכנות הפופולרית הזו.

## איך לעשות זאת

 נתחיל עם מודול JSON המאפשר לנו לקרוא ולכתוב נתונים בפורמט זה. נתחיל עם דוגמה פשוטה של יצירת אובייקט JSON והדפסתו למסך:

```Haskell
-- ייבוא המודול הנחוץ
import Text.JSON

-- ציון נתונים לאובייקט, במקרה זה שם וגיל
data Person = Person String Int

-- יצירת פונקציה הממירה את האובייקט לפורמט JSON
toJSON :: Person -> JSValue
toJSON (Person name age) = JSObject $ toJSObject [("name", JSString $ toJSString name), ("age", JSRational False $ fromInteger $ toInteger age)]

-- יצירת אובייקט חדש עם שם וגיל
person1 = Person "John" 30

-- הדפסת האובייקט בפורמט JSON
printJSON person1

-- תוצאה:
-- { "name":"John", "age":30 }
```

עם המודול הזה אנחנו יכולים לשלב את יכולות השפה החזקות לעבוד עם סטרינגים ומספרים כדי ליצור אובייקטים מורכבים ומותאמים.

## שקיפות לעומק

לעבוד עם JSON מתבצע באמצעות דוגמאות. תוכלו למצוא מבחר נרחב של דוגמאות בתיעוד של המודול כדי ללמוד יותר על קריאה, כתיבה וניתוח נתונים מסוג זה. ניתוח JSON כמו קבלת נתונים מפורמט זה לאובייקטים מוכנים לשימוש מתבצע עם פונקציות בשם `fromJSON`.

ניתן גם להמיר אוב