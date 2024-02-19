---
aliases:
- /he/elixir/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:00.340394-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E4\u05E2\u05E0\u05D5\u05D7 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 JSON \u05DC\u05DE\u05D1\u05E0\u05D9\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05D0\u05DC\u05D9\u05E7\u05E1\u05D9\
  \u05E8 (Elixir) \u05D9\u05DB\u05D5\u05DC\u05D4 \u05DC\u05EA\u05E4\u05E2\u05DC, \u05D5\
  \u05E1\u05D9\u05D3\u05D5\u05E8 \u05DE\u05D7\u05D3\u05E9 \u05E9\u05DC \u05DE\u05D1\
  \u05E0\u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC \u05D0\u05DC\u05D9\
  \u05E7\u05E1\u05D9\u05E8 \u05DC\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA JSON. \u05D6\u05D4\u2026"
lastmod: 2024-02-18 23:08:52.548093
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05E4\u05E2\u05E0\u05D5\u05D7 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 JSON \u05DC\u05DE\u05D1\u05E0\u05D9\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05D0\u05DC\u05D9\u05E7\u05E1\u05D9\
  \u05E8 (Elixir) \u05D9\u05DB\u05D5\u05DC\u05D4 \u05DC\u05EA\u05E4\u05E2\u05DC, \u05D5\
  \u05E1\u05D9\u05D3\u05D5\u05E8 \u05DE\u05D7\u05D3\u05E9 \u05E9\u05DC \u05DE\u05D1\
  \u05E0\u05D9 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC \u05D0\u05DC\u05D9\
  \u05E7\u05E1\u05D9\u05E8 \u05DC\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA JSON. \u05D6\u05D4\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם JSON כוללת פענוח של מחרוזות בפורמט JSON למבני נתונים שאליקסיר (Elixir) יכולה לתפעל, וסידור מחדש של מבני נתונים של אליקסיר לתוך מחרוזות JSON. זה חיוני לפיתוח אתרי אינטרנט, API-ים, וקבצי תצורה, מכיוון ש-JSON הוא פורמט החלפת נתונים מבוסס טקסט, קל משקל, עצמאי מבחינה שפתית ונמצא בשימוש נרחב בגלל פשטותו וקריאותו האנושית.

## איך לעשות:

באליקסיר, ניתן להשתמש בספריית `Jason`, בחירה פופולרית לפענוח ויצירת JSON. תחילה, הוסף את `Jason` לתלות של הפרוייקט שלך ב-`mix.exs`:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

לאחר מכן, הרץ את `mix deps.get` כדי להוריד את התלות.

### פענוח JSON:
כדי להמיר מחרוזת JSON למבני נתונים של אליקסיר:

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# פלט: %{"name" => "John", "age" => 30}
```

### יצירת JSON:
כדי להמיר מפת אליקסיר למחרוזת JSON:

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# פלט: {"age":25,"name":"Jane"}
```

### עבודה עם מבנים:
כדי להקודד מבנה של אליקסיר, עליך ליישם את הפרוטוקול `Jason.Encoder` עבור המבנה שלך. הנה דוגמה:

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# פלט: {"age":28,"name":"Mike"}
```

גישה פשוטה זו תעזור לך להתחיל לשלב עיבוד JSON ביישומי אליקסיר שלך, מה שמקל על החלפת נתונים בסביבות תכנות שונות.
