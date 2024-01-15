---
title:                "חיבור מחרוזות"
html_title:           "Gleam: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה
כתבה זו תעסוק בפעולת טקסט רב-שורתית בשם "Concatenation", שבו מתבצעת התחברות של שני טקסטים ליחד. נלמד למה זה חשוב ואיך ניתן לעשות זאת בשפת תכנות Gleam.

## איך לעשות זאת
ניתן להשתמש בפונקציית "concat" על מנת לחבר שני טקסטים בתוך סדרת תווים (string) אחת. נבצע עדכון למשתנה `result` על ידי הוספת הטקסט `Hello` לטקסט המקורי `World` ונדפיס את התוצאה:

```Gleam
let result = concat("Hello", "World")
```

תוצאה:
```
HelloWorld
```
אם נציין יותר משני טקסטים, נבצע חיבור של כולם יחד עם הטקסט המקורי:

```Gleam
let text1 = "How "
let text2 = "are "
let text3 = "you?"
let result = concat(text1, text2, text3)
```

תוצאה:
```
How are you?
```

## בינתחומי
נתחקר כיצד פעולת חיבור טקסטים נעשית בפנים שפת התכנות Gleam. המנגנון מבוסס על המימוש המובנה של הוויזט "string.append" בשפת Erlang, המאפשרת חיבור תווים לאובייקט כדי ליצור טקסט חדש.

## ראה גם
- תיעוד רשמי על פעילות "Concatenation" בגיאת הלידים של Gleam: [קישור](https://gleam.run/) 
- שפת התכנות Gleam באתר הרשמי: [קישור](https://gleam.run/)