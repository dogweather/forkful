---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים לפי תבנית היא פעולה בה מוחק המתכנת תווים מסוימים מתוך מחרוזת. זה מועיל לניקוי ועיבוד נתונים, כמו שינוי פורמט המחרוזת או הסרת תווים לא רצויים. 

## איך:
כאן יש דוגמאות לקוד שמראות איך למחוק תווים התואמים לתבנית ממחרוזת בשפת Gleam.

```Gleam
import gleam/string.{from_tuple, fold, from}

fn delete_pattern(text: String, pattern: String) -> String {
  fold(from_tuple(String.to_tuple(text)),
    fn(_, accumulator) {
      case from(pattern) {
        Ok(_) -> accumulator
        Error(_) -> append(from_tuple(accumulator), _)
      }
    },
    ""
  )
}

let text = "Hello, Gleam programmers!"
let pattern = ","

// remove commas
delete_pattern(text, pattern)  //-> "Hello Gleam programmers!"
```

## צוללים לעומק
למרות שהפונקציה `delete_pattern` שיצרנו בקוד שלנו מתאימה למחיקת תווים מסוימים, ישנם דרכים אחרות להשיג את אותו התוצאה בשפת Gleam. 

במקרה מסויים, ניתן לשקול להשתמש בפונקציות ושיטות אחרות כמו `filter`, `replace` ו`reduce`. בדרך כלל, הבחירה בין האפשרויות הללו מתבצעת לפי הדרישות הספציפיות של המשימה. 

## ראו גם
שפת Gleam מקנה למתכנתים כמה שיטות לעבוד עם מחרוזות. ניתן למצוא מידע נוסף על השימוש במחרוזות בGleam בכתובות האינטרנט הבאות: 

1. (Gleam String API)[https://docs.gleam.run/stdlib/string/]
2. (GitHub - Gleam Cookbook)[https://github.com/gleam-lang/gleam-cookbook#string] 
3. (Gleam String tutorial)[https://gleam.run/book/tour/strings.html]