---
title:                "ניתוח HTML"
date:                  2024-01-20T15:31:00.510449-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"

category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
פיענוח HTML מתייחס לתהליך שבו מתוכנת מנתחת מסמך HTML ומשיגה ממנו מידע סטרוקטורי. תוכניתנים עושים זאת כדי לשלוף נתונים, לאוטומט פעולות באינטרנט ולעבד דפי ווב.

## How to: (איך לעשות:)
ב-Elixir, אתה יכול להשתמש בספריה כמו Floki לפיענוח HTML בקלות. דוגמא לקוד:

```elixir
defmodule HTMLParser do
  def parse_html(html) do
    {:ok, document} = Floki.parse(html)
    Floki.find(document, "a")
    |> Enum.map(fn({_, attrs, _}) -> attrs end)
    |> Enum.map(&List.keyfind(&1, "href", 0))
  end
end

html_content = "<html><body><a href='https://example.com'>Link</a></body></html>"
links = HTMLParser.parse_html(html_content)
IO.inspect(links)
```

פלט לדוגמא:

```
[{"href", "https://example.com"}]
```

## Deep Dive (צלילה לעומק):
פעם, פענוח HTML היה אתגר גדול יותר עם ספריות מסורבלות ואי-הצמדות לתקנים של יצרני דפדפנים. היום, ספריות כמו Floki ב-Elixir נשענות על XPath וCSS selectors לשליפת נתונים ביעילות. אלטרנטיבות כוללות Nokogiri ב-Ruby וBeautifulSoup ב-Python. בחירת ספרייה תלויה בשפת התכנות ובדרישות הפרויקט.

## See Also (ראו גם):
* [Floki on Hex](https://hex.pm/packages/floki) - מידע על ספריית Floki, כולל מדריכים.
* [Programming Phoenix ≥ 1.4](https://pragprog.com/titles/phoenix14/programming-phoenix-1-4/) - פרקים על טיפול בHTML באמצעות Elixir וPhoenix.
* [Elixir Forum](https://elixirforum.com) - לשאלות וקהילה של מתכנתי Elixir.
