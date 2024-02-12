---
title:                "קריאת קובץ טקסט"
aliases:
- /he/elixir/reading-a-text-file.md
date:                  2024-01-20T17:54:23.150914-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

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
