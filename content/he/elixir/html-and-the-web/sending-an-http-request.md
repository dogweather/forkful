---
date: 2024-01-20 17:59:50.514434-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D6\u05D5\
  \ \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05E0\u05D5\u05E6\u05E8\
  \u05EA \u05EA\u05E7\u05E9\u05D5\u05E8\u05EA \u05D1\u05D9\u05DF \u05DC\u05E7\u05D5\
  \u05D7 \u05DC\u05E9\u05E8\u05EA \u05D1\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D8\u05E2\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD, \u05DC\u05E9\u05DC\u05D5\u05D7 \u05E4\u05D5\u05E8\u05DE\u05D9\u05DD\
  , \u05D5\u05DC\u05D1\u05E6\u05E2 API calls \u05DC\u05E9\u05D9\u05E8\u05D5\u05EA\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05D7\u05E7\u05D9\u05DD."
lastmod: '2024-03-13T22:44:38.768243-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D6\u05D5\
  \ \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05E0\u05D5\u05E6\u05E8\
  \u05EA \u05EA\u05E7\u05E9\u05D5\u05E8\u05EA \u05D1\u05D9\u05DF \u05DC\u05E7\u05D5\
  \u05D7 \u05DC\u05E9\u05E8\u05EA \u05D1\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D8\u05E2\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD, \u05DC\u05E9\u05DC\u05D5\u05D7 \u05E4\u05D5\u05E8\u05DE\u05D9\u05DD\
  , \u05D5\u05DC\u05D1\u05E6\u05E2 API calls \u05DC\u05E9\u05D9\u05E8\u05D5\u05EA\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05D7\u05E7\u05D9\u05DD."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
weight: 44
---

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
