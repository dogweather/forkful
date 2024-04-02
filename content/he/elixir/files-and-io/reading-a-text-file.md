---
date: 2024-01-20 17:54:23.150914-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1-Elixir \u05D4\u05D9\u05D0 \u05DC\u05E7\u05D7\u05EA \u05DE\u05D9\
  \u05D3\u05E2 \u05DE\u05E7\u05D5\u05D1\u05E5 \u05E2\u05DC \u05D4\u05D3\u05D9\u05E1\
  \u05E7 \u05D5\u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D5 \u05D1\u05EA\u05D5\
  \u05DB\u05E0\u05D4. \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D9 \u05D4\u05DE\u05D9\u05D3\u05E2 \u05E2\u05E9\
  \u05D5\u05D9 \u05DC\u05D4\u05DB\u05D9\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  , \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA, \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D4\
  \u05D9\u05E1\u05D8\u05D5\u05E8\u05D9\u05D9\u05DD, \u05D0\u05D5 \u05DE\u05E9\u05D9\
  \u05DE\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.802618-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1-Elixir \u05D4\u05D9\u05D0 \u05DC\u05E7\u05D7\u05EA \u05DE\u05D9\u05D3\
  \u05E2 \u05DE\u05E7\u05D5\u05D1\u05E5 \u05E2\u05DC \u05D4\u05D3\u05D9\u05E1\u05E7\
  \ \u05D5\u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D5 \u05D1\u05EA\u05D5\u05DB\
  \u05E0\u05D4. \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D9 \u05D4\u05DE\u05D9\u05D3\u05E2 \u05E2\u05E9\u05D5\
  \u05D9 \u05DC\u05D4\u05DB\u05D9\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D4\
  \u05D2\u05D3\u05E8\u05D5\u05EA, \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D4\u05D9\
  \u05E1\u05D8\u05D5\u05E8\u05D9\u05D9\u05DD, \u05D0\u05D5 \u05DE\u05E9\u05D9\u05DE\
  \u05D5\u05EA\u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 22
---

## What & Why? (מה ולמה?)
קריאת קובץ טקסט ב-Elixir היא לקחת מידע מקובץ על הדיסק ולהשתמש בו בתוכנה. אנחנו עושים את זה כי המידע עשוי להכיל נתונים, הגדרות, נתונים היסטוריים, או משימות לביצוע.

## How to: (איך לעשות:)
קוד פשוט לקריאת קובץ:

```elixir
defmodule FileReader do
  def read_file(file_path) do
    case File.read(file_path) do
      {:ok, contents} -> contents
      {:error, reason} -> "Error reading file: #{reason}"
    end
  end
end

# שימוש דוגמא
IO.puts FileReader.read_file("hello.txt")
```

פלט לדוגמא אם הקובץ נמצא ונקרא בהצלחה:

```
שלום עולם!
```

## Deep Dive (עיון עמוק)
קריאת קובצים היא אחת מהמשימות הבסיסיות במרבית שפות התכנות. Elixir משתמשת ב-File API שמובנית ב-Erlang, ששפתה היא אב הטכנולוגיה. יש גם דרכים אלטרנטיביות לקריאת קבצים כמו הזרמת הנתונים באמצעות `Stream`, שיכולה להיות חסכונית יותר בזיכרון עבור קבצים גדולים. תמיד כדאי לטפל בתקלות נפוצות כמו קובץ שלא קיים או הרשאות דרך תפיסת חריגות (exceptions) ותנאים.

## See Also (ראה גם)
- [HexDocs - File](https://hexdocs.pm/elixir/File.html)
- [Erlang's :file module documentation](http://erlang.org/doc/man/file.html)
