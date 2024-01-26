---
title:                "שימוש במנפה שגיאות"
date:                  2024-01-26T03:49:35.922289-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במנפה שגיאות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/using-a-debugger.md"
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