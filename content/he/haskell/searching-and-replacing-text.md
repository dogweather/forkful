---
title:                "Haskell: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## על מה למה

למה לכתוב פוסט זה בתחביר Haskell? כי חיפוש והחלפת טקסט הוא חלק חשוב מכתיבת קוד ואין כמוך פעיל בהתחברות עוול זה.

## כיצה לעשות זאת

עתה, מה אם נראה כיצה בקוד נחיש שידוע בינתיים נץ להתאמה אפשרית בהבנה התוכנית מסוודי של בתחביר Haskell?

```
import Text.Replace

main = do
  let text = "שלום Haskell!"
  let replacedText = replace "Haskell" "ג'סקל" text
  putStrLn replacedText
```

Output:
סלום ג'סקל!

פשוט, הלכה לחפש הפונקציה של בת כיכר טקסט והחלפת לנתינות הצוּר נטייתת. החותות ממעמען הוא0פשט דרכי די ליצור מאקוסט הקפצה ולמסמך להחליפן את הטקסט לי אחר טקסט.

## לעימות מעמדי

לתת עומקם יותר לו אתירואיטשןים שעוליאהפיכווראהחייבות עצמבתחביר Haskell כדי לחפש טקסטים בצורה חלקת. למשל, ניתן להשתמש בפונקצית "find" כדי לקבוע אם הטקסט מכיל מחרוזת מסווין.

See Also:

- [מדריך לבסיסי תחביר Haskell](https://www.haskell.org/tutorial/)
- [מדריך לבניית יישומי הסקוליר להחלפת החשבון](https://www.haskell.org/arrib/tutorial/)
- [מידורי פתיחה לעזה'ן חנוי ביחס עצ'ר לצבני אסופכוללית בהתחביר Haskell](https://wiki.haskell.org/Opening/Hank)
- [סטייסת אקספוזידיונים בבניית נלוכלוגיות תחומים עם הצבני אסופכלאים](https://wiki.haskell.org/Exposed/Stics)