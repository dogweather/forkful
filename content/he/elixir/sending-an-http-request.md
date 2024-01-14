---
title:                "Elixir: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

כדי לשלוט בתקשורת עם שרתים אחרים, נשתמש בפעולת HTTP כדי לשלוח בקשות ולקבל תגובות. זה נפוץ מאוד ומאפשר לנו ליצור תוכניות חכמות יותר שיוכלו לתקשםר עם סביבתם.

## איך לעשות זאת

ב- Elixir, ניתן לשלוח בקשת HTTP באמצעות חבילת `HTTPoison`. נתחיל עם ההתקנה:

```
def deps do
  [{:httpoison, "~> 1.7"}]
end
```

לאחר מכן נטען את החבילה בתוך הקובץ המחלקה שלנו:

```
[alex@localhost ~]$ iex -S mix
iex> {:ok, pid} = HTTPoison.start
{:ok, #PID<0.70.0>}
iex> {:ok, status, headers, body} = HTTPoison.get("https://google.com")
{:ok, 200,
[
  {"date", "Thu, 31 May 2012 20:27:50 GMT"},
  {"server", "Apache/2.2.14 (Ubuntu)"},
  {"last-modified", "Tue, 29 May 2012 18:37:16 GMT"},
  {"content-length", "656"},
  {"content-type", "text/html"}
],
"<!DOCTYPE html><html>...</html>"}
```

כאן אנו משתמשים בפונקציה `HTTPoison.get/2` כדי לשלוח בקשה GET לאתר של גוגל. נשים לב שאנו מקבלים מחזר ערך המכיל את הקוד המצב, הכותרות, והתוכן של התגובה.

תוכלו ליצור גם בקשות POST, PUT, ו-DELETE ע"י החיבור לאתרים בעזרת הפונקציות המתאימות של `HTTPoison`.

## חפירה עמוקה

נתאר כמה דברים שחשוב לדעת על שליחת בקשות HTTP:

- בקשות GET דורשות בעיקר לקרוא תוכן מהשרת, בעוד שבקשות POST, PUT ו-DELETE דורשות גם לעדכן את התוכן.
- בכל בקשה, אנו יכולים לצפות לקבלת תוכן לא מלא בגלל בעיות בקישוריות אינטרנט או בתשובה של השרת.
- כדי למנוע חסימה של בקשות מהשרת, נוכל לכיתוב על חיבורים קבועים ולשחרר אותם בסיום התיעוד.

## ראה גם