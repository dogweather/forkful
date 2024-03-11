---
date: 2024-01-26 03:49:35.922289-07:00
description: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D3\u05D9\u05D1\u05D0\u05D2\u05E8\
  \ \u05D1-Elixir \u05DB\u05D5\u05DC\u05DC \u05E9\u05DC\u05D1\u05D9\u05DD \u05E9\u05DC\
  \ \u05DC\u05E2\u05D1\u05D5\u05E8 \u05D3\u05E8\u05DA \u05D4\u05E7\u05D5\u05D3, \u05D1\
  \u05D3\u05D9\u05E7\u05EA \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD \u05D5\u05DE\u05E2\
  \u05E7\u05D1 \u05D0\u05D7\u05E8 \u05D6\u05E8\u05DE\u05D9\u05DD \u05DB\u05D3\u05D9\
  \ \u05DC\u05D3\u05E8\u05D5\u05E1 \u05D1\u05D0\u05D2\u05D9\u05DD. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05D4\u05D1\u05D9\u05DF \u05D0\u05EA \u05D4\u05DC\u05D0 \u05E6\
  \u05E4\u05D5\u05D9 \u05D5\u05DC\u05D4\u05D1\u05D8\u05D9\u05D7\u2026"
lastmod: '2024-03-11T00:14:12.204467-06:00'
model: gpt-4-0125-preview
summary: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D3\u05D9\u05D1\u05D0\u05D2\u05E8\
  \ \u05D1-Elixir \u05DB\u05D5\u05DC\u05DC \u05E9\u05DC\u05D1\u05D9\u05DD \u05E9\u05DC\
  \ \u05DC\u05E2\u05D1\u05D5\u05E8 \u05D3\u05E8\u05DA \u05D4\u05E7\u05D5\u05D3, \u05D1\
  \u05D3\u05D9\u05E7\u05EA \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD \u05D5\u05DE\u05E2\
  \u05E7\u05D1 \u05D0\u05D7\u05E8 \u05D6\u05E8\u05DE\u05D9\u05DD \u05DB\u05D3\u05D9\
  \ \u05DC\u05D3\u05E8\u05D5\u05E1 \u05D1\u05D0\u05D2\u05D9\u05DD. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05D4\u05D1\u05D9\u05DF \u05D0\u05EA \u05D4\u05DC\u05D0 \u05E6\
  \u05E4\u05D5\u05D9 \u05D5\u05DC\u05D4\u05D1\u05D8\u05D9\u05D7\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
שימוש בדיבאגר ב-Elixir כולל שלבים של לעבור דרך הקוד, בדיקת משתנים ומעקב אחר זרמים כדי לדרוס באגים. מתכנתים עושים זאת כדי להבין את הלא צפוי ולהבטיח שהיישומים שלהם פועלים כפי שתוכננו.

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
