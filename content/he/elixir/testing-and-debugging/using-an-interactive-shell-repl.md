---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
aliases:
- he/elixir/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:13:33.725768-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## מה ולמה?
מעטפת אינטראקטיבית, או REPL (לולאת קריאה-הערכה-הדפסה), מאפשרת לך לנסות קטעי קוד בזמן אמת. תוכנתי Elixir משתמשים ב-REPL, שנקרא IEx (Elixir אינטראקטיבי), לניסויים, איתור באגים, ולמידת השפה.

## איך לעשות:
כדי להפעיל את IEx, פתח את הטרמינל שלך והקלד `iex`. הנה טעימה:

```Elixir
iex> name = "תוכניתן Elixir"
"תוכניתן Elixir"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

הפלט צריך להציג הצבות משתנה, תוצאות פונקציה, ופונקציה אנונימית בפעולה.

## צלילה לעומק
מעטפת IEx הייתה חלק מ-Elixir מראשית ימיו. חוזה ואלים, יוצר Elixir, שאב השראה ממעטפות אינטראקטיביות של שפות אחרות כמו `python` של Python ו-`irb` של Ruby. למרות ש-IEx שותפה למכנות רבות עם אלו, היא נבנתה כדי להתמודד עם האופי המקבילי של Elixir ומשולבת לחלוטין עם יכולות ה-Erlang VM.

אלטרנטיבות ל-IEx באקוסיסטם של Erlang כוללות את `erl`, מעטפת ה-Erlang. אך IEx מספקת סביבה ידידותית יותר ל-Elixir, עם תכונות כמו השלמה אוטומטית נרחבת, היסטוריה, ועוזרים.

מעטפת REPL של IEx היא יותר ממגרש משחקים; היא יכולה להתחבר באופן חלק למערכת פועלת. זה קריטי לאיתור באגים ביישומים חיים. המימוש התחתון נשען על BEAM (ה-Erlang VM), ומבטיח שתכונות כמו החלפת קוד חמה נתמכות ישירות במעטפת.

## ראה גם
בדוק את אלה לקריאה נוספת ומשאבים:

- [תיעוד IEx של Elixir](https://hexdocs.pm/iex/IEx.html)
- [Elixir אינטראקטיבי (IEx) - מעטפת Elixir](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [תיעוד `erl` של Erlang](http://erlang.org/doc/man/erl.html)
- [למידת מעטפת ה-Interactive של Elixir](https://elixirschool.com/en/lessons/basics/iex_helpers/)
