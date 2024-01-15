---
title:                "מחיקת תווים התואמים למבנה"
html_title:           "Elixir: מחיקת תווים התואמים למבנה"
simple_title:         "מחיקת תווים התואמים למבנה"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

מחיקת תווים התואמים לתבנית יכולה להיות כלי מאוד שימושי בתהליכי עיבוד טקסט וניתוח מידע. זה יכול לסייע לנו לנקות או לעצבן תווים חסומים או שאינם רלוונטיים לעבודתנו, ולהשאיר רק את התווים החשובים והנחוצים.

## איך לבצע

השתמשו בפונקציית "String.replace" ונתיחת משפטים למחוק תווים התואמים לתבנית. כאן נוכל להגדיר גם את התבנית שנרצה לחפש ואת התווים החלופיים שנרצה להחליף בהם. 

```Elixir
#נתיחת משפטים ומחיקה מהתווים התואמים
str = "Hello, World!"
new_str = String.replace(str, ~r/world/i, "")
# Output: "Hello, !"
```

## מסע עמוק

כאשר מחקים תווים במבנה שורה אחת, ניתן להשתמש בפונקציות מתקדמות כמו "Regex.replace!" ולעבוד עם תבניות מורכבות יותר כדי להגביר את רמת הניקוי והביצועים. ניתן גם להשתמש בתפקיד של המשתנה "chars" כדי לנהל כל מיני תכונות ופעולות של מחרוזות.

```Elixir
#משתמשים ב- "chars" כדי למחוק תווים במבנה מורכב
str = "1, 2, 3, 4, 5"
new_str = Enum.into(str, %{"2" => "", "4" => ""}).chars |> IO.puts
# Output: "1, , 3, , 5"
```

## ראו גם

* [מאמר על פונקציית "String.replace" ב-Elixir](https://elixir-lang.org/docs/v1.8/elixir/String.html#replace/4)
* [מדריך לתבניות ב-Elixir](https://hexdocs.pm/elixir/master/Regex.html)