---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
aliases:
- /he/elixir/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:11.663825-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/sending-an-http-request-with-basic-authentication.md"
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
