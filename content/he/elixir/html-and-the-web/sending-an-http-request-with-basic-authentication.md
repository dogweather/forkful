---
date: 2024-01-20 18:02:11.663825-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05D8\u05DB\u05E0\u05D9\u05E7\u05D4 \u05DC\u05D4\u05E2\u05D1\u05E8\u05EA \u05E9\
  \u05DD \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D5\u05E1\u05D9\u05E1\u05DE\u05D0 \u05D1\
  \u05D1\u05E7\u05E9\u05D4 \u05DC\u05E9\u05E8\u05EA. \u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05D0\u05DE\u05EA \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\
  \ \u05D1\u05D0\u05D5\u05E4\u05DF \u05E4\u05E9\u05D5\u05D8 \u05D5\u05DE\u05D0\u05D5\
  \u05D1\u05D8\u05D7 \u05DB\u05D7\u05DC\u05E7 \u05DE\u05EA\u05D4\u05DC\u05D9\u05DB\
  \u05D9 API."
lastmod: '2024-03-11T00:14:12.195758-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05D8\u05DB\u05E0\u05D9\u05E7\u05D4 \u05DC\u05D4\u05E2\u05D1\u05E8\u05EA \u05E9\
  \u05DD \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D5\u05E1\u05D9\u05E1\u05DE\u05D0 \u05D1\
  \u05D1\u05E7\u05E9\u05D4 \u05DC\u05E9\u05E8\u05EA. \u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05D0\u05DE\u05EA \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\
  \ \u05D1\u05D0\u05D5\u05E4\u05DF \u05E4\u05E9\u05D5\u05D8 \u05D5\u05DE\u05D0\u05D5\
  \u05D1\u05D8\u05D7 \u05DB\u05D7\u05DC\u05E7 \u05DE\u05EA\u05D4\u05DC\u05D9\u05DB\
  \u05D9 API."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא טכניקה להעברת שם משתמש וסיסמא בבקשה לשרת. תוכניתנים עושים זאת כדי לאמת משתמשים באופן פשוט ומאובטח כחלק מתהליכי API.

## איך לעשות:
תחילה, נצטרך להתקין את החבילה `HTTPoison` על ידי הוספתה לקובץ `mix.exs`:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

ואז נריץ `mix deps.get` בטרמינל כדי להתקין את החבילה.

עכשיו, בואו נשלח בקשת HTTP עם אימות בסיסי:

```elixir
auth = :base64.encode("username:password")
headers = [{"Authorization", "Basic #{auth}"}]
url = "https://example.com/protected/resource"

case HTTPoison.get(url, headers) do
  {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
    IO.puts("Success! Response: #{body}")
  {:ok, %HTTPoison.Response{status_code: status_code}} ->
    IO.puts("Received non-200 response: #{status_code}")
  {:error, %HTTPoison.Error{reason: reason}} ->
    IO.puts("Error: #{reason}")
end
```

תוצאה לדוגמא:

```
Success! Response: {"message":"Hello, authenticated user!"}
```

## הצצה לעומק:
בנייה ושליחת בקשות עם אימות בסיסי היא אסטרטגיה שחייה איתנו עוד מראשית ימי HTTP. זה אינו הכי בטוח שיש, אבל זה פשוט ונפוץ במקרים שאין צורך ברמת אבטחה גבוהה במיוחד. האימות הבסיסי עובד על ידי שליחת שם המשתמש והסיסמא בקידוד Base64 בכותרת Authorization.

על אף פשטותו, קיימות גם אלטרנטיבות יותר מאובטחות כמו אימות דיגיטלי (Digest Authentication), OAuth, ו-Token based authentication. הבחירה באימות תלויה בדרישות האבטחה ובצרכים הספציפיים של המערכת.

אגב, ל-Elixir יש חבילות אחרות שיכולות לשלוח בקשות HTTP, כמו `Tesla` ו`hackney`. `HTTPoison` עוגנת ב-`hackney` בעצמה ומספקת ממשק נוח ואלסטי לשימוש.

## ראה גם:
- [מדריך HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [מסמך קידוד Base64 ב-Elixir](https://hexdocs.pm/elixir/Base.html)
- [מידע על אימות בסיסי ב-MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [HTTP Basic Authentication - RFC 7617](https://tools.ietf.org/html/rfc7617)
