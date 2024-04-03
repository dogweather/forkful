---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:33.519353-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Elixir, \u0E04\
  \u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\
  \u0E23\u0E32\u0E23\u0E35 `Jason`, \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E22\u0E2D\u0E14\u0E19\u0E34\u0E22\
  \u0E21\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\
  \ JSON \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07 JSON \u0E01\
  \u0E48\u0E2D\u0E19\u0E2D\u0E37\u0E48\u0E19, \u0E40\u0E1E\u0E34\u0E48\u0E21 `Jason`\
  \ \u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19 dependencies\u2026"
lastmod: '2024-03-17T21:57:55.874501-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Elixir, \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 `Jason`, \u0E0B\u0E36\
  \u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E40\u0E25\u0E37\u0E2D\u0E01\
  \u0E22\u0E2D\u0E14\u0E19\u0E34\u0E22\u0E21\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07 JSON \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E2A\
  \u0E23\u0E49\u0E32\u0E07 JSON \u0E01\u0E48\u0E2D\u0E19\u0E2D\u0E37\u0E48\u0E19,\
  \ \u0E40\u0E1E\u0E34\u0E48\u0E21 `Jason` \u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\
  \u0E19 dependencies \u0E02\u0E2D\u0E07\u0E42\u0E1B\u0E23\u0E40\u0E08\u0E01\u0E15\
  \u0E4C\u0E43\u0E19 `mix.exs`."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## วิธีการ:
ใน Elixir, คุณสามารถใช้ไลบรารี `Jason`, ซึ่งเป็นตัวเลือกยอดนิยมสำหรับการแปลง JSON และการสร้าง JSON ก่อนอื่น, เพิ่ม `Jason` เข้าไปใน dependencies ของโปรเจกต์ใน `mix.exs`:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

จากนั้น, รัน `mix deps.get` เพื่อดึง dependency นั้นเข้ามา

### การแปลง JSON:
เพื่อแปลงสตริง JSON เป็นโครงสร้างข้อมูลของ Elixir:

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# ผลลัพธ์: %{"name" => "John", "age" => 30}
```

### การสร้าง JSON:
เพื่อแปลงแผนที่ของ Elixir เป็นสตริง JSON:

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# ผลลัพธ์: {"age":25,"name":"Jane"}
```

### การทำงานกับ Structs:
เพื่อ encode โครงสร้างข้อมูลของ Elixir, คุณต้องปรับใช้โปรโตคอล `Jason.Encoder` สำหรับโครงสร้างข้อมูลของคุณ นี่คือตัวอย่าง:

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# ผลลัพธ์: {"age":28,"name":"Mike"}
```

วิธีการนี้จะช่วยให้คุณเริ่มต้นการบูรณาการการประมวลผล JSON เข้ากับแอพพลิเคชัน Elixir ของคุณ, ทำให้สามารถแลกเปลี่ยนข้อมูลได้ในสภาพแวดล้อมการเขียนโปรแกรมที่หลากหลาย
