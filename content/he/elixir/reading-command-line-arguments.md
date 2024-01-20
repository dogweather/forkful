---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
ערכים של command line הם מידע אותו המשתמש מעביר לתוכנה דרך ה CLI (Command Line Interface). מתכנתים נוהגים לשלב קריאת ערכים של command line כדי להציג תוצאות מבוססות-אינפוט ולשלוט על תהליכים פנימיים של התוכנה.

## איך לקרוא:
הנה משל לקוד של Elixir שקורא ערכי command line:
```Elixir
 defmodule MyApp do
  def main(args) do
    IO.inspect(args)
  end
 end
```
אם ניקח לדוגמא את הפלט:
```bash
$ elixir my_app.exs arg1 arg2 arg3
["arg1", "arg2", "arg3"]
```
הקוד הפשוט לעיל מדפיס את כל הארגומנטים שהמשתמש האריך.

## צלילה עמוקה
בעבר, פרוטוקולים שונים נמנעו למנות את נתוני ה-CLI, ביניהם FORTRAN ו-Cobol. אך בעם הגאולה של שפות מודרניות יותר כמו Perl ו-Python, מיצאה את דרך האינפוט שלה שלמדנו להכיר. אליקסיר, שפה מודרנית שיוצאת משפת Erlang, נקייה ויעילה מדי כדי לא לטמון למשתמש אפשרות האינפוט.

אלטרנטיבות לקריאת ארגומנטים מן ה-command line כוללות קריאת מידע מקבצים או בהמתנה לקלט ממשתמש במהלך הריצה, אך שיטות אילו מורכבות יותר ואינן תמיד מתאימות לצרכים. בנוסף לכך, קיימות ספריות חיצוניות שמספקות אפשרויות לניהול ארגומנטים נוספות.

## ראה גם
תוכל ללמוד עוד על ערכי command line באליקסיר בקישורים הבאים:
- [מסמכי האליקסיר הרשמיים](https://elixir-lang.org/getting-started/io-and-the-file-system.html#command-line-arguments)