---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה זה ולמה? (What & Why?)
השרשור של מחרוזות הוא פעולה שמאחדת שתי מחרודת או יותר לאחת. מתכנתים משתמשים בזה לבנייה נוחה ואלגנטית של מחרוזות מרכיבים שונים.

## כיצד לעשות את זה: (How to:)
הדרך הכי קלה להשרשר מחרוזות בהסקל היא באמצעות האופרטור `++`
```Haskell
ghci> let greeting = "שלום " ++ "עולם"
ghci> print greeting
"שלום עולם"
```
אפשר גם להשתמש בפונקציה `concat` כדי להשרשר רשימה של מחרוזות
```Haskell
ghci> let colors = concat ["אדום", " ירוק", " כחול"]
ghci> print colors
"אדום ירוק כחול"
```

## צלילה עמוקה (Deep Dive)
האופרטור `++` בהסקל למעשה מממש רעיון שנקרא 'מונואיד'. במתמטיקה, מונואיד הוא מערכת שבה יש פעולה אחת שמאחדת פריטים, ויש יחידה שמשמשת כאיבר ניטרלי. בהסקל, הפונקציה `mappend` מוגדרת ברוב הסוגים למימוש של האופרטור `++`.

לגבי חלופות, אפשר להשתמש גם ב`mconcat`, שהיא גרסה של `concat` שעובדת עם `mappend`.

לעניין הביצועים, כל פונקציה שמשתמשת בפעולת `++` מתאימה לשרשור מחרוזות קצרות, אבל לא תהיה יעילה עם מחרוזות ארוכות מכיוון שהיא דורשת סריקה של הרשימה השמאלית למקום הסיום שלה.

## ראה גם (See Also)
1. [Learn You a Haskell - String Concatenation](http://learnyouahaskell.com/starting-out#an-intro-to-lists)
2. [Real World Haskell - Efficient List Concatenation](http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html#idp140448)
3. [Haskell Wiki - Difference between ++ and :](https://en.wikibooks.org/wiki/Haskell/Lists_and_tuples#Difference_between_.2B.2B_and_:)