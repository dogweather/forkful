---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:57.210743-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A TOML\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C (parsing) \u0E41\u0E25\u0E30\
  \u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \ TOML (Tom's Obvious, Minimal Language) \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49 Elixir\u2026"
lastmod: '2024-03-17T21:57:55.876482-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A TOML\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C (parsing) \u0E41\u0E25\u0E30\
  \u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \ TOML (Tom's Obvious, Minimal Language) \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49 Elixir\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\
  \u0E0A\u0E49\u0E21\u0E31\u0E19\u0E43\u0E19\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\
  \u0E04\u0E48\u0E32 \u0E40\u0E1E\u0E23\u0E32\u0E30\u0E27\u0E48\u0E32 TOML \u0E19\u0E31\
  \u0E49\u0E19\u0E2D\u0E48\u0E32\u0E19\u0E07\u0E48\u0E32\u0E22, \u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\
  \u0E4C\u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22 \u0E41\u0E25\u0E30\u0E21\u0E35\u0E01\
  \u0E32\u0E23\u0E08\u0E31\u0E1A\u0E04\u0E39\u0E48\u0E01\u0E31\u0E1A\u0E42\u0E04\u0E23\
  \u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E41\u0E1A\
  \u0E1A\u0E41\u0E2E\u0E0A(ha\u0161a)\u0E44\u0E14\u0E49\u0E14\u0E35."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## วิธีการ:
ประการแรก, เพิ่ม TOML parser ไปยัง dependencies ของคุณใน mix ตัวอย่างนี้ใช้ `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

อ่านไฟล์ TOML:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

เพื่อแปลงข้อมูล Elixir เป็น TOML:

```elixir
data = %{title: "TOML Example", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

ตัวอย่างผลลัพธ์:

```elixir
"title = \"TOML Example\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## การดำน้ำลึก
TOML ถูกสร้างโดย Tom Preston-Werner, ผู้ร่วมก่อตั้ง GitHub, สำหรับใช้ในไฟล์การกำหนดค่า มันถูกออกแบบมาเพื่อให้ง่ายกว่า XML และละเอียดกว่า YAML ในขณะที่รักษาความเอกลักษณ์

ทางเลือกอื่น ๆ รวมถึงไฟล์ JSON, YAML, และ INI, แต่ละอย่างมีข้อดีข้อเสียของตัวเองในแง่ของความสามารถในการอ่านและการจับคู่โครงสร้างข้อมูล TOML โดดเด่นในการแสดงข้อมูลแบบตารางและการจัดกลุ่มข้อมูลที่ซับซ้อน

ใน Elixir, การจัดการ TOML ขึ้นอยู่กับไลบรารีการแปลงโค้ด (decoding) และการเข้ารหัส (encoding) ซึ่งทำการแปลงสายข้อมูล TOML เป็นแมปของ Elixir และกลับกัน การแยกวิเคราะห์ทํางานโดยการจับคู่กฎไวยากรณ์ของ TOML และแปลงเป็นชนิดข้อมูลของ Elixir การเข้ารหัสทำการตรงกันข้ามโดยการแมปชนิดข้อมูลของ Elixir กลับไปเป็นไวยากรณ์ TOML ที่ถูกต้อง

## ดูเพิ่มเติม
- ภาษา TOML: https://toml.io/en/
- `toml-elixir` ที่เก็บบน GitHub: https://github.com/bitwalker/toml-elixir
- รายละเอียดแพ็คเกจ Hex สำหรับ `toml-elixir`: https://hex.pm/packages/toml_elixir
