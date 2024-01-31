---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T17:59:50.514434-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP זו התהליך שבו נוצרת תקשורת בין לקוח לשרת באינטרנט. תכנתים עושים זאת כדי לטעון נתונים, לשלוח פורמים, ולבצע API calls לשירותים מרוחקים.

## איך לעשות:
ב-Elixir, נעשה שימוש בחבילות צד שלישי כמו `HTTPoison` או `Tesla` לשליחת בקשות HTTP. דוגמה עם `HTTPoison`:

```elixir
# קודם כל, הוסיפו את HTTPoison לתלותים בקובץ mix.exs שלכם:
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# אחרי זה, עשו את הקריאה הבאה:

HTTPoison.get!("https://jsonplaceholder.typicode.com/posts")
```

תוצאה:
```
%HTTPoison.Response{
  status_code: 200,
  body: "[{...}]"
}
```
## צלילה עמוקה:
שליחת בקשות HTTP היא תופסת מקום מרכזי מאז תחילת האינטרנט. ב-Elixir, חבילות כמו `HTTPoison` מבוססות על `hackney`, בעוד `Tesla` מציעה middleware וגמישות רבה יותר. `HTTPoison` מוצלחת בזכות ה-API הפשוט שלה, ו-Tesla בזכות הגמישות שהיא מספקת. גרסא לקריאה מתויכת ולשליחה לא חסומה של בקשות גם קיימת עם `:httpc`, המובנית ב-Erlang OTP.

## ראה גם:
- [HTTPoison GitHub](https://github.com/edgurgel/httpoison)
- [Tesla GitHub](https://github.com/teamon/tesla)
