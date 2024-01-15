---
title:                "שליחת בקשת http"
html_title:           "Elixir: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

בעזרת שליחת בקשת HTTP ניתן לתקשר עם שרתים רחוקים ולקבל מידע ונתונים מהאינטרנט. פעולה זו חיונית ליצירת תקשורת בין אתרים ואפליקציות.

## איך לבצע

```Elixir
# אימפורטים נחוצים
iex> require HTTPoison
iex> require Jason

# שליחת קשיחה עם HTTP GET
iex> HTTPoison.get("https://example.com")
{:ok,
 %HTTPoison.Response{
   body: "<!DOCTYPE html>\n<html>\n<head>\n<title>Example Domain</title>\n<style ......",
   headers: [
     {"Content-Type", "text/html; charset=UTF-8"}, {"Pragma", "no-cache"},
     {"Date", "Thu, 30 Sep 2021 00:00:00 GMT"}, {"Server", "gws"},
     {"Cache-Control", "private"},
     {"Set-Cookie",
      "test_cookie=CookieValue; expires=Thu, 30-Sep-2021 01:00:00 GMT; Max-Age=3600; path=/; domain=example.com"}
   ],
   request_url: "https://example.com",
   status_code: 200
 }}

# שליחת קשיחה עם HTTP POST
iex> HTTPoison.post("https://example.com", Jason.encode!(%{name: "John", age: 30}))
{:ok,
 %HTTPoison.Response{
   body: "<!DOCTYPE html>\n<html>\n<head>\n<title>Example Domain</title>\n<style ......",
   headers: [
     {"Content-Type", "text/html; charset=UTF-8"}, {"Pragma", "no-cache"},
     {"Date", "Thu, 30 Sep 2021 00:00:00 GMT"}, {"Server", "gws"},
     {"Cache-Control", "private"},
     {"Set-Cookie",
      "test_cookie=CookieValue; expires=Thu, 30-Sep-2021 01:00:00 GMT; Max-Age=3600; path=/; domain=example.com"}
   ],
   request_body: "%7B%22name%22%3A%22John%22%2C%22age%22%3A30%7D",
   request_headers: [["content-type", "application/x-www-form-urlencoded"]],
   request_method: :post,
   request_url: "https://example.com",
   status_code: 200
 }}
```

## תהליך מעמיק

כאשר אנו שולחים בקשת HTTP, אנחנו בעצם מבקשים מהשרת לשלוח לנו תוכן חזרה. זה מאפשר לנו לקבל מידע ונתונים בקלות מהאינטרנט ולהשתמש בהם לביצוע פעולות נוספות. ניתן להשתמש בפונקציות כמו `get/2` ו-`post/3` של הספרייה HTTPoison על מנת לשלוח בקשות באינטרנט. על מנת להפעיל יישום זה, עלינו להתקין את הספרייה ולהוסיף את החבילות הנחוצות לפעולת שליחת בקשות HTTP.

## ראה גם

- [HexDocs: HTTPoison](https://