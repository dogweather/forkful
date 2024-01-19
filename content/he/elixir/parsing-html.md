---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?

פיענוח HTML הוא התהליך שבמהלכו אנו ממירים מסמך HTML למבנה נתונים שניתן לעבד. מתכנתים מבצעים אותו לניתוח התוכן של עמודי אינטרנט ולאיסוף מידע מהם.

## איך עושים?

נשתמש בספרייה Floki של אליקסיר:

```Elixir
{:floki, "~> 0.26"}

html = "<div><p>Hello, world!</p></div>"
{"div", _, children} = Floki.parse(html) |> hd()
IO.inspect(children)
```

כאן אנו טוענים את הספרייה, מפרשים מסמך HTML פשוט ומדפיסים את התוכן שוּנמצא בתוך תג <div>. תצוגה מקדימה:

```Elixir
[%{"p" => ["Hello, world!"]}]
```

## צלילה מעמיקה

מעבר לFloki, קיימות ספריות אחרות לפיענוח HTML באליקסיר, כמו הספרייה מוס (Mochiweb). Floki הוא האפשרות הפופולרית ביותר בגלל פקודות הפשוטות שלה לאיסוף מידע.
ישנה חשיבה למפרשים באליקסיר, שיש אותה בליבת השפה, שמתמקדת בקלות שימוש, ניתוח פשוט, וראיית השיפורים בפעולה.

## עוד לעיון

[תיעוד Floki](https://hexdocs.pm/floki/readme.html)
[פרויקט Mochiweb ב-GitHub](https://github.com/mochi/mochiweb)