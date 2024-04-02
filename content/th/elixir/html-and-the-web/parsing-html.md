---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:30.299226-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \ HTML \u0E14\u0E49\u0E27\u0E22 Elixir \u0E21\u0E35\u0E01\u0E32\u0E23\u0E14\u0E36\
  \u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E40\u0E2D\u0E01\u0E2A\
  \u0E32\u0E23 HTML \u0E40\u0E1B\u0E47\u0E19\u0E2B\u0E25\u0E31\u0E01 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\
  \u0E07\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A\u0E40\
  \u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E44\u0E14\u0E49\u0E41\u0E1A\u0E1A\u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E15\u0E34\u0E01, \u0E14\u0E36\u0E07\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:55.849037-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \ HTML \u0E14\u0E49\u0E27\u0E22 Elixir \u0E21\u0E35\u0E01\u0E32\u0E23\u0E14\u0E36\
  \u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\u0E40\u0E2D\u0E01\u0E2A\
  \u0E32\u0E23 HTML \u0E40\u0E1B\u0E47\u0E19\u0E2B\u0E25\u0E31\u0E01 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\
  \u0E07\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A\u0E40\
  \u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E44\u0E14\u0E49\u0E41\u0E1A\u0E1A\u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E15\u0E34\u0E01, \u0E14\u0E36\u0E07\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
weight: 43
---

## คืออะไร & ทำไม?

การแยกข้อมูล HTML ด้วย Elixir มีการดึงข้อมูลจากเอกสาร HTML เป็นหลัก โปรแกรมเมอร์ทำสิ่งนี้เพื่อให้สามารถโต้ตอบกับเว็บเพจได้แบบโปรแกรมเมติก, ดึงข้อมูล, หรือทำการโต้ตอบกับเว็บแบบอัตโนมัติ เปิดโอกาสให้แอพพลิเคชั่นเข้าใจและใช้เนื้อหาเว็บแบบไดนามิกได้

## วิธีทำ:

Elixir ด้วยโมเดลการทำงานพร้อมกันที่แข็งแกร่งและปรัชญาการเขียนโปรแกรมแบบฟังก์ชันไม่ได้รวมความสามารถในการแยกข้อมูล HTML เข้ามาอยู่ด้วย อย่างไรก็ตาม คุณสามารถใช้ไลบรารีของบุคคลที่สามที่นิยมเช่น `Floki` สำหรับวัตถุประสงค์นี้ Floki ทำให้การแยกข้อมูล HTML เป็นไปอย่างใจเอื้อมและมีประสิทธิภาพ เป็นผลมาจากคุณสมบัติการเขียนแพทเทิร์นและการใช้งานพั-pipe ของ Elixir 

ก่อนอื่น เพิ่ม Floki ลงใน dependencies ของคุณใน mix.exs:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

จากนั้น ทำการรัน `mix deps.get` เพื่อติดตั้ง dependency ใหม่

ตอนนี้ ลองแยกข้อมูลจากสตริง HTML เรียบง่ายเพื่อดึงข้อมูลออกมา ประการแรก เราจะมองหาชื่อหัวเรื่องภายในแท็ก `<h1>`:

```elixir
html_content = """
<html>
  <body>
    <h1>สวัสดี, Elixir!</h1>
    <h1>อีกหัวข้อหนึ่ง</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**ผลลัพธ์ตัวอย่าง:**

```elixir
["สวัสดี, Elixir!", "อีกหัวข้อหนึ่ง"]
```

เพื่อทำความเข้าใจมากขึ้น สมมติว่าคุณต้องการดึงข้อมูลลิงค์ (`<a>` tags) พร้อมกับแอตทริบิวต์ href ของมัน นี่คือวิธีการทำ:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">เว็บไซต์อย่างเป็นทางการของ Elixir</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**ผลลัพธ์ตัวอย่าง:**

```elixir
[{"เว็บไซต์อย่างเป็นทางการของ Elixir", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

วิธีการนี้ช่วยให้คุณสามารถนำทางและแยกข้อมูลเอกสาร HTML ได้อย่างมีประสิทธิภาพ ทำให้งานดึงข้อมูลและจัดการข้อมูลเว็บในแอพพลิเคชั่น Elixir เป็นไปอย่างราบรื่น
