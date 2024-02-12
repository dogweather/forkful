---
title:                "הורדת דף אינטרנט"
aliases: - /he/elixir/downloading-a-web-page.md
date:                  2024-01-20T17:44:12.576605-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
להוריד דף אינטרנט זה פשוט לחלץ את התוכן שלו באופן תכנותי. תכניתנים עושים את זה כדי לעבד את המידע, לגרד נתונים, או לבצע בדיקות אוטומטיות.

## איך לעשות:
אנו נשתמש בספריית HTTPoison לשליחת בקשת GET וקבלת תוכן האתר.

```elixir
defmodule PageDownloader do
  def download(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, "Failed with status code #{status_code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end
end

# שימוש במודול:
{:ok, content} = PageDownloader.download("http://example.com")
```

תוצאת הדוגמה:
```
<i>תוכן של דף http://example.com...</i>
```

## צלילה לעומק:
לפני HTTPoison, תכניתנים השתמשו ב-HTTP ספריות כמו ibrowse ו-httpc (מובנה ב-Erlang). ב-HTTPoison משתמשים בספרייה של Hackney, שמספק תמיכה טובה בהתחברויות מרובות וניתוח שגיאות. חלופות נוספות כוללות ספריות כמו Tesla ו-HTTPotion.

בחירה ב-HTTPoison נבעת מן הפופולריות שלה, הקלות שבשימוש וההתממשקות הידידותית למתכנת. זה חשוב לזכור שעל מנת להשתמש ב-HTTPoison, צריך להוסיף אותה כתלות בקובץ `mix.exs`.

## ראו גם:
- מסמכי HTTPoison: https://hexdocs.pm/httpoison
- Hackney GitHub: https://github.com/benoitc/hackney
- רשימת ספריות שימושיות נוספות ל-Elixir: https://github.com/h4cc/awesome-elixir
