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

## מה ולמה?

שליחת בקשת HTTP היא פעולה שבה מתבצעת בעזרת תקשורת רשת, בה נמצאת הודעה ייחודית המבקשת משאב מסוים מהשרת. מתכנתים משתמשים בפעולה זו כדי לקבל מידע או לבצע פעולות על השרת.

## איך לעשות זאת?

הנה דוגמא פשטנית לשליחת בקשת HTTP באמצעות Elixir:

```Elixir
HTTP.request(:get, "https://www.google.com") 
|> HTTPoison.fetch! 
|> IO.inspect
```

פלט הקוד הינו תשובת השרת מסוג HTTP בפורמט מסוים.

## הורדה עמוקה

בעבור היסטוריית התוכנית, שליחת בקשת HTTP התחילה ככלי חשוב בתקשורת רשת בשנות השמונים של המאה ה-20. כיום, ישנן אלטרנטיבות רבות כמו AJAX ו-REST API שמשמשות כדי לשלוח בקשות HTTP.

המימוש של Elixir של שליחת בקשת HTTP מצויין ותומך בכמה פרוטוקולים שונים כמו HTTP, HTTPS ו-HTTPS2. הוא גם מספק כמה מתודות וכלים נוספים כדי לשפר את תהליך התקשורת ואת ביצועי התכנית.

## ראו גם

קישורים למקורות נוספים:

- [מדריך רשמי של Elixir לשליחת בקשת HTTP](https://hexdocs.pm/elixir/HTTP.html)
- [פרויקט HTTPoison - ספריית אינטרנט שנכתבה באמצעות Elixir](https://github.com/edgurgel/httpoison)
- [מאמר "AJAX - התוספות והטבעים המתוחכמים" מאת ג'רמי קיטלן](https://www.adaptivepath.org/ideas/ajax-new-approach-web-applications/)