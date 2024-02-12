---
title:                "עבודה עם JSON"
date:                  2024-02-03T19:23:00.340394-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
