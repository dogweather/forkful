---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:15.056108-07:00
description: "\u05E4\u05E2\u05E0\u05D5\u05D7 HTML \u05D1\u05D0\u05DC\u05D9\u05E7\u05E1\
  \u05D9\u05E8 \u05DB\u05D5\u05DC\u05DC \u05D0\u05EA \u05D7\u05D9\u05DC\u05D5\u05E5\
  \ \u05D4\u05DE\u05D9\u05D3\u05E2 \u05DE\u05DE\u05E1\u05DE\u05DB\u05D9 HTML. \u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E4\u05E9\u05E8 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05D0\u05E7\u05E6\u05D9\u05D4 \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9\u05EA \u05E2\
  \u05DD \u05D3\u05E4\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8, \u05DC\u05D2\
  \u05E8\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D0\u05D5 \u05DC\u05D0\u05D5\
  \u05D8\u05DE\u05D8 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05D1\u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8,\u2026"
lastmod: '2024-03-13T22:44:38.769820-06:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05E2\u05E0\u05D5\u05D7 HTML \u05D1\u05D0\u05DC\u05D9\u05E7\u05E1\
  \u05D9\u05E8 \u05DB\u05D5\u05DC\u05DC \u05D0\u05EA \u05D7\u05D9\u05DC\u05D5\u05E5\
  \ \u05D4\u05DE\u05D9\u05D3\u05E2 \u05DE\u05DE\u05E1\u05DE\u05DB\u05D9 HTML."
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
weight: 43
---

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
