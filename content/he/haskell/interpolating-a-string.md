---
title:                "שדרגת מחרוזת"
html_title:           "Haskell: שדרגת מחרוזת"
simple_title:         "שדרגת מחרוזת"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

ההתאפסות של מחרוזת היא כלי חזק ב- Haskell שמאפשר למתכנתים להכניס משתנים אל מחרוזות בצורה נוחה ויעילה. למרבה המזל, ישנן מספר שיטות לעשות זאת בתוכניות שלנו.

## כיצד לעשות:

כדי להשתמש בהתאפסות של מחרוזת ב- Haskell נשתמש בפונקציות פשוטות כמו `printf` ו- `putStrLn`. באמצעות אלו, אנו יכולים להכניס משתנים מסוגים שונים בתוך מחרוזת על ידי ציון שלהם כהפרמטרים. לדוגמה:

```Haskell
myString = printf "Hello %s, you are %d years old!" "John" 25
putStrLn myString
```

תוצאה התוכנית תהיה: "Hello John, you are 25 years old!"

על מנת ליישם זאת בתוכנית שלנו, עלינו להוסיף את הטקסט "import Text.Printf" לראש הקובץ הנוכחי שלנו.

## מסע לעומק:

בעבר, כדי להשתמש בהתאפסות של מחרוזת ב- Haskell היה עלינו להשתמש בספריות חיצוניות כגון Fmt ו- Printed שפיתוחן היה קשה וזמן לימודן ארוך. אבל הבעיה כיום נמצאת בצורה פשיטה וניתן להשתמש בפונקציות פשוטות כדי לפענח את ההתאפסות של מחרוזת ב- Haskell.

כמו בכל שפת תכנות אחרת, ישנן חלופות להשתמש בהתאפסות של מחרוזת ב- Haskell כמו שימוש בפונקציות `show` ו- `print` שכולן יכולות לבצע את אותה המשימה, אבל יכולות להיות יותר מסורבלות לשימוש. אם ברצוננו להשתמש בפתרון אחר, עלינו לבדוק את האפשרויות ולבחור את הפתרון הטוב ביותר בשבילנו.

## ראה גם:

למידע נוסף ודוגמאות נוספות על השתמש בהתאפסות של מחרוזת ב- Haskell, אנו ממליצים לך לבדוק את המדריכים והמשאבים הבאים:

- [Haskell Wiki](https://wiki.haskell.org/Formatting_strings)
- [Learn You a Haskell](http://learnyouahaskell.com/input-and-output)
- [Real World Haskell](http://book.realworldhaskell.org/read/io.html)