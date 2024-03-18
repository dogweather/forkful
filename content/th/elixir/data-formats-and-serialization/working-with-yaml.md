---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:23.237214-06:00
description: "YAML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 YAML Ain't Markup\
  \ Language \u0E40\u0E1B\u0E47\u0E19\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E01\
  \u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E25\u0E44\u0E25\u0E0B\u0E4C\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\
  \u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C\u2026"
lastmod: '2024-03-17T21:57:55.873636-06:00'
model: gpt-4-0125-preview
summary: "YAML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 YAML Ain't Markup\
  \ Language \u0E40\u0E1B\u0E47\u0E19\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E01\
  \u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E25\u0E44\u0E25\u0E0B\u0E4C\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\
  \u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

YAML ย่อมาจาก YAML Ain't Markup Language เป็นมาตรฐานการซีเรียลไลซ์ข้อมูลที่อ่านได้โดยมนุษย์ ซึ่งใช้กันอย่างแพร่หลายสำหรับไฟล์การตั้งค่าและการแลกเปลี่ยนข้อมูลระหว่างภาษาที่มีโครงสร้างข้อมูลที่แตกต่างกัน โปรแกรมเมอร์ใช้มันเพราะความเรียบง่ายและความสามารถในการแสดงข้อมูลแบบชั้นเชิงที่ซับซ้อนได้ง่าย

## วิธีการ:

Elixir ไม่มีการรองรับ YAML โดยตรง อย่างไรก็ตาม คุณสามารถใช้ไลบรารีของบุคคลที่สาม เช่น `yamerl` หรือ `yaml_elixir` เพื่อทำงานกับ YAML ที่นี่ เราจะเน้นที่ `yaml_elixir` เนื่องจากมีความง่ายในการใช้งานและคุณสมบัติที่ครอบคลุม

ก่อนอื่น เพิ่ม `yaml_elixir` ไปยัง dependencies ของคุณใน mix.exs:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

จากนั้น รัน `mix deps.get` เพื่อดึง dependency ใหม่

### การอ่าน YAML

หากมีไฟล์ YAML ง่ายๆ เช่น `config.yaml` ที่มีลักษณะดังนี้:

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

คุณสามารถอ่านไฟล์ YAML นี้และแปลงเป็นแผนที่ของ Elixir ได้ดังนี้:

```elixir
defmodule Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# ตัวอย่างการใช้งาน
Config.read()
# ผลลัพธ์: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### การเขียน YAML

เพื่อเขียนแผนที่กลับไปยังไฟล์ YAML:

```elixir
defmodule ConfigWriter do
  def write do
    content = %{
      database: %{
        adapter: "mysql",
        username: "root",
        password: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", content)
  end
end

# ตัวอย่างการใช้งาน
ConfigWriter.write()
# นี่จะสร้างหรือเขียนทับ `new_config.yaml` ด้วยเนื้อหาที่ระบุ
```

สังเกตว่า `yaml_elixir` ช่วยให้การแปลระหว่างไฟล์ YAML และโครงสร้างข้อมูล Elixir เป็นไปอย่างตรงไปตรงมา ทำให้เป็นตัวเลือกที่ยอดเยี่ยมสำหรับโปรแกรมเมอร์ Elixir ที่จำเป็นต้องทำงานกับข้อมูล YAML
