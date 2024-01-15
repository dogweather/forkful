---
title:                "שינוי צנצנת לגיבוכי מחשב"
html_title:           "Elixir: שינוי צנצנת לגיבוכי מחשב"
simple_title:         "שינוי צנצנת לגיבוכי מחשב"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה

כדי לשנות את הכתיב של מחרוזת מיירב באמצעות אטריביוט הפונקציה `String.capitalize/1`.

## איך לעשות

השייט הבא מציג דוגמאות לשימוש בפונקציה `String.capitalize/1` ואת התוצאות הצפויות:

```Elixir
iex> String.capitalize("elixir")
"Elixir"

iex> String.capitalize("hebrew")
"Hebrew"

iex> String.capitalize("eLiXir")
"ELiXir"

iex> String.capitalize("123")
"123"
```

הפונקציה `String.capitalize/1` מגדירה את האות הראשונה במחרוזת כאות גדולה עבור מילים בלבד. אם המחרוזת מכילה מספרים או תווים אחרים, הפונקציה תשים את האות הראשונה באות גדולה ותשאיר את תואמים הנותרים כמו שהם.

## חקירה מעמיקה

ניתן להשתמש בפונקציה `String.capitalize/1` גם כדי לשנות את כל המילים במחרוזת לאות גדולה. כדי לעשות זאת, נצטרך לכתוב קוד קצת יותר מורכב:

```Elixir
iex> "capitalize all words in this string"
|> String.split(" ")
|> Enum.map(&String.capitalize/1)
|> Enum.join(" ")
"Capitalize All Words In This String"
```

יש לשים לב כי אנו משתמשים בפונקציות נוספות כגון `String.split/2` ו- `Enum.join/2` כדי לבצע את השינוי על כל המילים במחרוזת כאחד.

## ראה גם

- [פונקציות מחרוזת נוספות ב-Elixir](https://hexdocs.pm/elixir/String.html)