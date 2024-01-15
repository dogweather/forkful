---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Elixir: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה

בשנים האחרונות, אינטרנט הפך לחלק בלתי נפרד מחיינו. אנו משתמשים בו לכל מיני דברים, כגון קניות, תורניות ואף בעבודה. אחד הדברים הכי נפוצים שאנו עושים באינטרנט הוא שליחת בקשות HTTP, שמאפשרות לנו ליצור תקשורת בין המכשיר שלנו לבין השרת של האתר. אם אתם מנסים לשלוח בקשת HTTP עם אימות בסיסי, המאמץ הכי נפוץ בכדי לוודא שרק המשתמשים המורשים יכולים לקבל מידע שמגיע מהשרת.

## כיצד

```Elixir
defmodule Example do
  use HTTPoison
  def send_request(url, username, password, query) do
    HTTPoison.get(url, [basic_auth: {username, password}, body: query])
  end
end
```

בקוד המנהל, אנו מגדירים את המודול החדש שלנו ושולפים את HTTPoison, אחד הספריות הכי פופולריות ב-Elixir לשותפי HTTP. אז, בפונקציה send_request שלנו, אנו משתמשים בפונקציה get מספריית HTTPoison כדי לשלוח בקשת GET בכוונה לכתובת האתר שחברתי כדי לספק את הנתונים הנחוצים כדי לצרף אותו כסתם בנקודת הנתינה. בנוסף, אנו מוסיפים פרמטר שמציג לספק כי הבקשה ממכילה אימות בסיסי עם שם משתמש וסיסמה, כדי להבטיח שרק המשתמשים המורשים יכולים לקבל את הנתונים.

```Elixir
{:ok,
 %HTTPoison.Response{
   body: "[{\"name\": \"John\", \"age\": 25}]",
   headers: [{"Content-Type", "application/json"}],
   request_url: "https://www.example.com/query",
   status_code: 200
 }}
```

כשאנו מריצים את הפונקציה שלנו עם הפרמטרים המתאימים, אנו מקבלים תשובה מהשרת בפורמט JSON, עם נתונים כמו שעשינו בבקשה GET. אם הבקשה לא נכשלת, אנו מקבלים קוד