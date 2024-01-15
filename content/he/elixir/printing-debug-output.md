---
title:                "הדפסת פלט ניתוח שגיאות"
html_title:           "Elixir: הדפסת פלט ניתוח שגיאות"
simple_title:         "הדפסת פלט ניתוח שגיאות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## למה: 
הדפסת פלטי דיבאג הינה כלי מקצועי וחשוב בעבודת התכנות. היא מאפשרת לפיתוחנים לזהות ולתקן בעיות בקוד בצורה יעילה ומהירה.

## איך לעשות זאת:
באמצעות שפת התכנות אליקסיר, ניתן להדפיס פלטי דיבאג באמצעות הפונקציה IO.inspect. להלן דוגמא לשימוש בפונקציה עם פרמטרים שונים:

```Elixir
IO.inspect("Hello, world!") 
# output: "Hello, world!"

IO.inspect([1, 2, 3], label: "List") 
# output: "List: [1, 2, 3]"

IO.inspect({:ok, :success}, label: "Response", pretty: true) 
# output: "Response: {:ok, :success}"
```

## מעמקים נמוכים:
בנוסף להדפסת פלטי דיבאג, שפת אליקסיר מציעה כמה כלים נוספים לניתוח בעיות בקוד. למשל, ניתן להשתמש בפונקציה IO.inspect/2 כדי להדפיס את כל הפרמטרים ברשומת זמן מבנה יותר מפורטת. בנוסף, ניתן להשתמש בפונקציה IO.inspect/1 כדי להדפיס את העץ המלא של אובייקט או משתנה מסוים.

## ראה גם:
- [התחלה מהירה עם אליקסיר: הדפסת פלטי דיבאג](https://elixir-lang.org/getting-started/debugging.html)
- [מסמך ההדפסה של שפת אליקסיר](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [דוגמאות לשימוש ב-debugging באליקסיר](https://blog.appsignal.com/2020/06/03/debugging-in-elixir-the-basics.html)