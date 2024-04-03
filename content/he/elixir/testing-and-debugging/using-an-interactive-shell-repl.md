---
date: 2024-01-26 04:13:33.725768-07:00
description: "\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\
  \u05D8\u05D9\u05D1\u05D9\u05EA, \u05D0\u05D5 REPL (\u05DC\u05D5\u05DC\u05D0\u05EA\
  \ \u05E7\u05E8\u05D9\u05D0\u05D4-\u05D4\u05E2\u05E8\u05DB\u05D4-\u05D4\u05D3\u05E4\
  \u05E1\u05D4), \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DA \u05DC\u05E0\u05E1\
  \u05D5\u05EA \u05E7\u05D8\u05E2\u05D9 \u05E7\u05D5\u05D3 \u05D1\u05D6\u05DE\u05DF\
  \ \u05D0\u05DE\u05EA. \u05EA\u05D5\u05DB\u05E0\u05EA\u05D9 Elixir \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1-REPL, \u05E9\u05E0\u05E7\u05E8\u05D0 IEx (Elixir\
  \ \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9),\u2026"
lastmod: '2024-03-13T22:44:38.775983-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\
  \u05D8\u05D9\u05D1\u05D9\u05EA, \u05D0\u05D5 REPL (\u05DC\u05D5\u05DC\u05D0\u05EA\
  \ \u05E7\u05E8\u05D9\u05D0\u05D4-\u05D4\u05E2\u05E8\u05DB\u05D4-\u05D4\u05D3\u05E4\
  \u05E1\u05D4), \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DA \u05DC\u05E0\u05E1\
  \u05D5\u05EA \u05E7\u05D8\u05E2\u05D9 \u05E7\u05D5\u05D3 \u05D1\u05D6\u05DE\u05DF\
  \ \u05D0\u05DE\u05EA."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
weight: 34
---

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
