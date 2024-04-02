---
date: 2024-01-20 17:44:12.576605-07:00
description: "\u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05D3\u05E3 \u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8 \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D7\u05DC\
  \u05E5 \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05DF \u05E9\u05DC\u05D5 \u05D1\u05D0\
  \u05D5\u05E4\u05DF \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9. \u05EA\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05D0\u05EA \u05D4\u05DE\u05D9\u05D3\
  \u05E2, \u05DC\u05D2\u05E8\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D0\u05D5\
  \ \u05DC\u05D1\u05E6\u05E2 \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D0\u05D5\u05D8\
  \u05D5\u05DE\u05D8\u05D9\u05D5\u05EA."
lastmod: '2024-03-13T22:44:38.771395-06:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8 \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D7\u05DC\u05E5\
  \ \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05DF \u05E9\u05DC\u05D5 \u05D1\u05D0\u05D5\
  \u05E4\u05DF \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9. \u05EA\u05DB\u05E0\u05D9\u05EA\
  \u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05D0\u05EA \u05D4\u05DE\u05D9\u05D3\u05E2\
  , \u05DC\u05D2\u05E8\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D0\u05D5 \u05DC\
  \u05D1\u05E6\u05E2 \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D0\u05D5\u05D8\u05D5\
  \u05DE\u05D8\u05D9\u05D5\u05EA."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
weight: 42
---

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
