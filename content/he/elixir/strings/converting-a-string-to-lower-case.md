---
title:                "המרת מחרוזת לאותיות קטנות"
aliases: - /he/elixir/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:11.247241-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה המרה של מחרוזת לאותיות קטנות ולמה זה נחוץ? להמיר מחרוזת לאותיות קטנות זה לשנות את כל האותיות במחרוזת לפורמט הקטן שלהן. תוכניתנים עושים זאת לצורך אחידות, השוואה בלתי תלויה רישיות, וקלט משתמש.

## How to:
ב-Elixir, משתמשים בפונקציה `String.downcase/1` כדי להמיר מחרוזת לאותיות קטנות:

```elixir
iex> my_string = "Shalom, OLAM!"
iex> String.downcase(my_string)
"shalom, olam!"
```

קל ופשוט. השתמש ב`String.downcase/1` וקיבלת את התוצאה.

## Deep Dive
ההמרה לאותיות קטנות היא לא פשוטה כמו שזה נשמע. יש לשקול תרבויות ושפות שונות עם סט של תווים מיוחדים. במקרה של Elixir, שהיא שפה תומכת UTF-8, היא מתמודדת עם המרות תווים לא אסקי גם יחד.

לפני שהייתה הפונקציה `String.downcase/1`, תוכניתנים היו צריכים להמיר את המחרוזות באופן ידני. זה היה תהליך מסורבל ופגיע לשגיאות.

כיום, הפונקציה `String.downcase/1` משתמשת בספריית Elixir עצמה או בספריות מערכת ההפעלה כדי לבצע המרות מורכבות של תווים בצורה יעילה ונכונה.

## See Also
- [Elixir String module](https://hexdocs.pm/elixir/String.html)
- [Unicode standards](https://unicode.org/standard/standard.html)
- [Elixir School: Strings](https://elixirschool.com/en/lessons/basics/strings/)
