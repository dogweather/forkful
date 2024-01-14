---
title:    "Haskell: חילוץ תת-מחרוזות"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

# למה

 למה להשתתף בחלוץ substrings ב-Haskell? 

 בתור מתכנתים, אנו מתמקדים תמיד בביצועים ונפתול בקוד שלנו באמצעות כלים וטכניקות מתקדמות. חלץ substrings הוא כלי שיעזור לנו למצוא חלקים ספציפיים בתוך מחרוזת ולבצע פעולות מורכבות עליהם, וכך יוצר קוד איכותי ומנומסט.

 # איך לעשות את זה

 נלמד כיצד לחלץ substrings ב-Haskell באמצעות הקוד הבא:

 ```Haskell
import Data.List (isInfixOf)

substr :: String -> String -> Maybe String
substr str sub = if isInfixOf sub str
                  then Just sub
                  else Nothing

main :: IO ()
main = do 
    let text = "זהו דוגמא למחרוזת בעברית"
    print $ substr text "מחרוזת" -- מחזיר את החלק "מחרוזת" מהמחרוזת המקורית
    print $ substr text "אינדקס" -- לא מצא את התת-מחרוזת ולא מחזיר כלום
 ```

 כאן אנחנו משתמשים בפונקציית `isInfixOf` ממודול הסטנדרטי של Data.List כדי לבדוק אם תת-מחרוזת הנתונה נמצאת בתוך המחרוזת המקורית. אם היא נכונה, אנו מחזירים את התת-מחרוזת, אחרת אנו מחזירים `Nothing`. בדוגמאות הקוד ניתן לראות שהפונקציה `substr` מחזירה תוצאה לפי הציפייה.

 # מצא תת-מחרוזת

 אבל איך עובד רקורסיה? אנחנו יכולים להשתמש בפונקציה `take` כדי לחלק את המחרוזת למובנה בסוגרים ואז להשתמש בפונקציה `tail` כדי לפרוס את המחרוזת נוספת. ניתן לראות את זה בקוד הבא:

 ```Haskell
substr :: String -> String -> Maybe String
substr str sub = if isInfixOf sub str
                  then Just sub
                  else if length str < length sub
                       then Nothing
                       else substr (tail str) sub

main :: IO ()
main = do
    let text = "גם זה דוגמא למחרוזת"
    print $ substr text "זה" -- מחזיר את התת-מחרוזת "זה