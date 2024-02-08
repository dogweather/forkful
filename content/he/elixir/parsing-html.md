---
title:                "פיענוח HTML"
aliases:
- he/elixir/parsing-html.md
date:                  2024-02-03T19:12:15.056108-07:00
model:                 gpt-4-0125-preview
simple_title:         "פיענוח HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

פענוח HTML באליקסיר כולל את חילוץ המידע ממסמכי HTML. תכנתים עושים זאת כדי לאפשר אינטראקציה תכנותית עם דפי אינטרנט, לגרד נתונים, או לאוטמט פעולות באינטרנט, מה שמאפשר ליישומים להבין ולנצל תוכן ברשת באופן דינמי.

## איך לעשות:

אליקסיר, עם מודל הבזמניות הנחושה ופרדיגמת התכנות הפונקציונלית שלו, אינה כוללת יכולות פענוח HTML מובנות. עם זאת, ניתן להשתמש בספריות צד שלישי פופולריות כמו `Floki` למטרה זו. Floki הופכת פענוח HTML לאינטואיטיבי ויעיל, תוך ניצול תכונות התאמת הדפוסים וההעברה של אליקסיר.

ראשית, הוסף את Floki לתלויות mix.exs שלך:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

לאחר מכן, הרץ את `mix deps.get` כדי להתקין את התלות החדשה.

עכשיו, בואו נפרס אכוח רטט פשוט לחילוץ נתונים. נחפש את הכותרות בתוך תגיות `<h1>`:

```elixir
html_content = """
<html>
  <body>
    <h1>שלום, אליקסיר!</h1>
    <h1>כותרת נוספת</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**פלט לדוגמא:**

```elixir
["שלום, אליקסיר!", "כותרת נוספת"]
```

להעמקה נוספת, נניח שאתם רוצים לחלץ קישורים (תגיות `<a>`) יחד עם האטריביוטים שלהם. הנה איך אפשר לעשות זאת:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">האתר הרשמי של אליקסיר</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**פלט לדוגמא:**

```elixir
[{"האתר הרשמי של אליקסיר", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

גישה זו מאפשרת לכם לנווט ולפרס מסמכי HTML ביעילות, והופכת משימות של חילוץ ומניפולציה של נתונים מהאינטרנט לפשוטות ביישומי אליקסיר.
