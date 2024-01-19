---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?

מחיקת תווים שמתאימים לתבנית היא בעצם הליך שבו אנחנו "מנקים" מחרוזת, על ידי הסרת תווים מסוימים ממנה. התכנתים משתמשים בזה כאשר יש צורך למנוע קלט לא תקני או לייעל את ניתוח הנתונים.

## איך לעשות:

הנה איך נמחקים תווים בהעזרת שימוש ברגולר עם מודול re של Python.
 
```Python
import re

str = "Hello, עולם!"
pattern = "[, הםע!]"
new_str = re.sub(pattern, "", str)

print(new_str)
```

פלט שגרה הקוד:

```Python
llool
```

## שיעור מעמיק

(1) הוסרו התווים שהתאימו לתבנית מהמחרוזת המקורית: ההיסטוריה מאחורי זה היא אופטימיזציה של מערכי מחשוב. בעבר, המידע נשמר באופן פיזי על גבי תקליטים עגלגלים, אז היה נחוץ למניעת מידע לא רלוונטי.
  
(2) גרסאות אחרות: ניתן להשתמש במחרוזת's replace () פונקציה עבור תווים בודדים. מחרוזת יכולה להכיל רשימה של תוצאות תחליף לכל תו במחרוזת מקור. הבעיה היא שזה לא יעשה התאמה לתבניות או ביטויים מורכבים יותר.

(3) פרטים לגבי מימוש: כאשר מחרוזת נפרסת לאובייקטים של תווים, ניתן לבצע "האם מתאים" (תנאי) על תוים בודדים. אם התנאי מתקיים, התו ממחק. אם לא, התו משאר. מכאן המילה "filter".

## ראה גם

- W3Schools Python re.sub(): `https://www.w3schools.com/python/ref_func_re_sub.asp`
- Python strip(): `https://www.w3schools.com/python/ref_string_strip.asp`
- Python translate(): `https://www.w3schools.com/python/ref_string_translate.asp`