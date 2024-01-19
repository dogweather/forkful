---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?

חיבור מחרוזות הוא הביטוי המתאר את התהליך של החרזה של שניים או יותר מחרוזות אילו לאחת. מתכנתים עושים את זה כדי לנהל, להשוות, לשלב ולעצב מקראים, הודעות ועוד צורות של מידע טקסטואלי באופן דינאמי.‬

## ‫‬כיצד לחבר מחרוזות:

באליקסיר, אתה יכול לחבר מחרוזות  באמצעות אופרטור `<>`. להלן דוגמה:
```Elixir
string_1 = "שלום"
string_2 = ", עולם"
IO.puts string_1 <> string_2
```
פלט אפשרי: `שלום, עולם`.

## ‪‬צלילה עמוקה:

‫‬חיבור מחרוזות הוא טכניקה יעילה ומהירה שהתפתחה בהקשר של תכנות ממוחשב. החלופות לחיבור מחרוזות כוללת אינטרפולאציה של מחרוזות, אבל הן נוטות להיות קצת פחות יעילות מבחינה ביצועית. מנגנון החיבור של אליקסיר מחבר בין מחרוזות ללא לשנות את המחרוזות המקוריות, מה שאומר שמספק חיבורים בצורה מהירה ויעילה.‬

## ‪‬ראה גם:

‫
-‬ [Elixir School - Basic Operation](https://elixirschool.com/en/lessons/basics/modules/‪)
- [Elixir Official Documentation - String concatenation](https://hexdocs.pm/elixir/String.html#concat/1‪)
‬‬