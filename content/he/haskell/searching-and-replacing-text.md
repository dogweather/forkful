---
title:    "Haskell: חיפוש והחלפה של טקסטים"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה:

מחיפוש והחלפת טקסט הוא כלי חשוב ונפוץ בכתיבת קוד בכל שפת תכנות. זה מאפשר לנו לשנות טקסט בקלות ולקבל תוצאות מדויקות ומהירות. כתיבה בטכניקות שונות כגון פאטרנים, רגולריות, והמלצות הופכים את החשיבה על כמה דרכים ובכך מחזקים את היכולת הכתיבתית השליטה שלנו בשפת הקוד.

## איך לעשות:

מטרתו של כלי החיפוש וההחלפה של האסקל הינה למצוא עץ מאופסת או שקופית בטקסט ולהחליף אותם במילים אחרות. כדי לעשות זאת, נשתמש בפונקציות נוספות מהילד הם "import Data.List" ו "import Data.Char" לשלב פשוט ביניהם כדי ליצור קוד נקי ומובנה.

במקום העבודה, אנו נממש קוד ב-Cسקל כדי להמחיש את התהליך הדורש החיסמת של ימצא את הטקסט הנבחר ולהחליף אותו במילה נוספת.

 ```Haskell
import Data.List
import Data.Char

searchAndReplace :: String -> String -> String -> String
searchAndReplace old new str = intercalate new (splitOn old str)

main = do
    putStrLn "הכנס את הטקסט בו יבוצע החיפוש:"
    str <- getLine
    putStrLn "הכנס את המילה הישנה:"
    old <- getLine
    putStrLn "הכנס את המילה החדשה:"
    new <- getLine
    putStrLn (searchAndReplace old new str)
 ```
 
לדוגמה, אם נכניס את הטקסט הבא: "אני מאמינה שאני יודעת איך לתרגם קוד ב-Haskell. אך לא נראה בודאות כל הזמן...", עם המילים הישנות: "אני" והמילה החדשה היא "אנחנו", הקוד יחזיר: "אנחנו מאמינה שאנחנו יודעת איך לתרגם קוד ב-Haskell. אך לא נראה בודאות כל הזמן...".

## תהות מעמיקה:

אם אנחנו רוצים ל