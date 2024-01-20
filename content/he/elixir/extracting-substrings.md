---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
אקסטרקציה של תת-מחרוזות היא הפעולה שבה אנו מקבלים מנגנון כלשהו בתוך מחרוזת. המתכנתים עושים את זה כי מדובר בשיטה מהירה ונוחה לשפצוף נתונים שהם כבר חשופים להם במחרוזת.

## איך לעשות:
באליקסיר, אתה יכול למנות את המספרים מתחילת המחרוזת (אפס כמובן) או מסוף המחרוזת (ראשון). הנה דוגמה:

```Elixir
iex> String.slice("Hello, world!", 0..4)
"Hello"

iex> String.slice("Hello, world!", -6..-2)
"world"
```

## צלילה עמוקה
בעבר, המתכנתים הינו צריכים לכתוב ממשק מבצע לטיפול בכל פונקציה של מחרוזת, אך אליקסיר מנותח את זה באופן מרשים עם מודולים מתקדמים. ישנם דרכים אחרות למשוך מחרוזות תתיות, אבל הרוב די מסתמך על המתודולוגיה שמציעה Elixir. מבחינת פרטי ההצגה, Elixir משתמשת באלגוריתם קל ומהיר שמאפשר לה למשוך תת מחרוזות באופן מהיר ויעיל.

## לראות גם
למידע נוסף על אליקסיר ומחרוזת של מחרוזות, קישורים מומלצים הם:
1. [תיעוד מקורי של Elixir String](https://hexdocs.pm/elixir/String.html)
2. [הסבר מעולה על מחרוזות באליקסיר](https://elixirschool.com/en/lessons/basics/strings/)
3. [הסבר על מחרוזות באליקסיר בStackOverflow](https://stackoverflow.com/questions/23094884/how-to-get-a-substring-in-elixir).