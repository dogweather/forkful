---
date: 2024-01-20 17:54:23.150914-07:00
description: "How to: (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA:) \u05E7\u05D5\
  \u05D3 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\
  \u05D1\u05E5."
lastmod: '2024-04-05T21:53:40.091752-06:00'
model: gpt-4-1106-preview
summary: "(\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA:) \u05E7\u05D5\u05D3\
  \ \u05E4\u05E9\u05D5\u05D8 \u05DC\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\
  \u05E5."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 22
---

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
