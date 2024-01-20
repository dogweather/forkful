---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה זה & למה?

שליחת בקשת HTTP עם אוטנטיקצייה בסיסית היא בסופו של דבר דרך להעביר מידע מהמחשב שלך לשרת על ידי שליחת המידע באופן מאובטח. הסיבה לביצוע זה מגיעה בדרך כלל מהצורך להבטיח שהמידע שאנחנו שולחים לשרת הוא מאובטח ולא נחשף לצדדים שלישיים.

## איך לבצע:

באיליקסיר, אפשר לשלוח בקשת HTTP עם אוטנטיקציה בפרוטוקול בסיסי באמצעות ה-library `HTTPoison`. בדוגמה הבאה, אנחנו שולחים GET request לאתר כלשהו.

Elixir:
```Elixir
defmodule Example do
  require HTTPoison
  def send_request do
    url = "https://example.com"
    HTTPoison.get!(url, %{}, hackney: [basic_auth: {'username', 'password'}])
  end
end
```
## עומק מידע:
שליחת בקשות HTTP עם אוטנטיקציה מבסיסית היא נוסחאות ישנה שנוצרה עם מאמצים ראשונים של ה-HTTP. למרות זאת, הרעיון הכללי נשאר רלוונטי. ישנם חלופות, קרי כאלה שמשתמשות בטוקנים כמו JWT, או שליחת username ו-password ב-body של הבקשה במקום ב-header. החסרונות הראשיים של שיטה זו הם:
1.   הסיסמאות משודרות בתוך ה-headers של ה-request, מה שיכול להיות מסוכן אם החיבור לא מאובטח (לא על SSL / TLS).
2.   אם השרת לא תומך ב-HTTP Basic Authentication, המידע יהיה זמין לצפיה.

## ראה גם:

[הגדרת HTTP Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
[הדרכה של Elixir HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
[עמוד ה-HTTP Basic Authentication של Mozilla](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)