---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?

הורדת דף אינטרנט היא התהליך שבו המחשב מאחזר נתונים מהשרת ומאחסנו במקום מקומי. התכנתים מתעסקים בזה כדי לנתח נתונים, לבצע בדיקות מכנות, או לשמור עותק מקומי של הדף.

## איך?

שניתן להשתמש בHTTPoison בשפת התכנות Elixir כדי להוריד דף אינטרנט:

```Elixir
defmodule Downloader do
  def download(url) do
    case HTTPoison.get(url) do
      {:ok, response} ->
        IO.puts("download successful")
        {:ok, response.body}
      {:error, reason} -> 
        IO.puts("download failed")
        {:error, reason}
    end
  end
end
```

בדוגמה הזו, `HTTPoison.get(url)` יחזיר אחת משני האפשרויות- התגובה של השרת או שגיאה.

## עומק הנושא

יכול להיות ששמעת ששפת Elixir משתמשת ב-HTTPoison במקום בספרייה אחרת. זו קבלה תכליתית שהפעילה את הקהל המתכנת החדש ל־Elixir. ספריות אחרות, כמו HTTPotion או HTTPipe, קיימות אך לא פופולאריות כך שקשה יותר למצוא תמיכה או קוד דוגמה עבורן.

## לעיון נוסף

רוצה ללמוד יותר על HTTPoison? נסה את [הדוקומנטציה הרשמית](https://hexdocs.pm/httpoison/readme.html).
נערך כבר לשפת Elixir? התחל [כאן](https://elixir-lang.org/getting-started/introduction.html). להם גם פורום תמיכה מצוין [Elixir Forum](https://elixirforum.com).