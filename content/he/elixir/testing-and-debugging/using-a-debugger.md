---
date: 2024-01-26 03:49:35.922289-07:00
description: "Elixir \u05DE\u05D2\u05D9\u05E2 \u05E2\u05DD \u05D3\u05D9\u05D1\u05D0\
  \u05D2\u05E8 \u05D2\u05E8\u05E4\u05D9 \u05DE\u05D5\u05D1\u05E0\u05D4 \u05D1\u05E9\
  \u05DD `:debugger`. \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\
  \u05D5, \u05EA\u05E6\u05D8\u05E8\u05DA \u05DC\u05D4\u05E4\u05E2\u05D9\u05DC \u05D0\
  \u05D5\u05EA\u05D5 \u05D5\u05DC\u05D4\u05EA\u05D7\u05D1\u05E8 \u05DC\u05EA\u05D4\
  \u05DC\u05D9\u05DA \u05D4\u05E8\u05E5 \u05E9\u05DC\u05DA. \u05E8\u05D0\u05E9\u05D9\
  \u05EA, \u05D4\u05D1\u05D8\u05D7 \u05E9-`:debugger` \u05DE\u05D5\u05E4\u05E2\u05DC\
  \ \u05D1\u05EA\u05D5\u05DA \u05E1\u05E9\u05DF `iex`:\u2026"
lastmod: '2024-03-13T22:44:38.781264-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u05DE\u05D2\u05D9\u05E2 \u05E2\u05DD \u05D3\u05D9\u05D1\u05D0\u05D2\
  \u05E8 \u05D2\u05E8\u05E4\u05D9 \u05DE\u05D5\u05D1\u05E0\u05D4 \u05D1\u05E9\u05DD\
  \ `:debugger`."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA"
weight: 35
---

## איך לעשות זאת:
Elixir מגיע עם דיבאגר גרפי מובנה בשם `:debugger`. כדי להשתמש בו, תצטרך להפעיל אותו ולהתחבר לתהליך הרץ שלך.

ראשית, הבטח ש-`:debugger` מופעל בתוך סשן `iex`:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

עכשיו, פרש את המודול שברצונך לבצע עליו דיבג:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

אתה יכול להגדיר נקודת עצירה:
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

ואז, הפעל את הפונקציה שלך כדי להגיע לנקודת העצירה ולעבור דרך הקוד שלך:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# הדיבאגר יעצור את הביצוע בשורה עם נקודת העצירה
```

## צלילה עמוקה
לפני `:debugger` של Elixir, Erlang סיפקה את הדיבאגר ש-Elixir משתמשת בו; הוא עמיד ומצוין בטיפול בתהליכים מקבילים, נקודת חוזק של מכונת Erlang VM (BEAM). בניגוד לדיבאגרים אחרים, `:debugger` לא מאפשר שינוי של משתנים במהלך הריצה, בשל הטבע האימוטבלי של הנתונים ב-Elixir. כחלופות, יש לך את `IEx.pry` שמאפשר לך לעצור ביצוע ולקפוץ ל-REPL בכל נקודה בקוד שלך, שיכול להיות מאוד שימושי.

בעוד ש-`:debugger` טוב לממשק גרפי, יש כאלו שיעדיפו את הכלי המובנה `:observer` שגם הוא מציע בדיקת תהליכים ומדדי מערכת, אף על פי שלא ממוקד במיוחד בשלבים דרך הקוד. קהילת Elixir גם תורמת כלים כמו `visualixir` ו-`rexbug`, המרחיבה את האקוסיסטם של כלי דיבוג מעבר לברירות המחדל.

## ראה גם
- מדריך התחלתי רשמי של Elixir על דיבוג: https://elixir-lang.org/getting-started/debugging.html
- תיעוד `:debugger` של Erlang: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- דיונים בפורום Elixir על טכניקות דיבוג: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
