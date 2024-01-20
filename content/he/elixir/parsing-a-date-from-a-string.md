---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
לנתח תאריך ממחרוזת הוא תהליך של חילוץ המידע הרלוונטי ממחרוזת, והאפשרות להציגו או להשתמש בו באופן יותר יעיל. מתכנתים עושים את זה כדי ליצור אינטראקציות יותר עשירות, דינמיות ואישיות 

## איך עושים את זה: 
הקוד הבא מדגים את התהליך:
```Elixir
{:ok, date} = Date.from_iso8601("2016-01-01")

IO.puts(to_string(date))
```

תוצאת הקוד הנ"ל תהיה:

"2016-01-01"

## בדיקה מעמיקה:
חלק מההקשר ההיסטורי לעבודה עם תאריכים במחרוזת הוא שמתחילה מיישומים על מחשבים מקומיים, בהם היו תצורות אישיות של תאריכים וזמנים. 

כבר היו מספר בחינות שאפשרנו למתכנתים לנתח תאריכים ממחרוזות, כולל Timex וCalendar. עם זאת, הטרק ביבליוטקה של Elixir כוללת כיום יכולת מובנית לנתח תאריכים.

מעתה והלאה, נכונה עכשיו, יש יכולת מובנית בשפת Elixir לנתח לתאריך ממחרוזת, מה שמקנה לנו את היכולת לבצע זאת בצורה יעילה ונוחה מאוד.

## ראה גם:
1. [Elixir Date Documentation](https://hexdocs.pm/elixir/Date.html) - התיעוד הרשמי של המודול 'Date' בשפת Elixir.
2. [Introduction to Elixir](https://elixir-lang.org/getting-started/introduction.html) - מבוא לשפת Elixir
3. [Elixir School](https://elixirschool.com/en/) - לימוד Elixir interactive למתכנתים של כל הרמות.