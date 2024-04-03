---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:46.982414-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E0A\u0E49\u0E44\
  \u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 `HTTPoison` \u0E02\u0E2D\u0E07 Elixir \u0E21\
  \u0E31\u0E19\u0E40\u0E23\u0E35\u0E22\u0E1A\u0E07\u0E48\u0E32\u0E22 \u0E43\u0E0A\u0E49\
  \u0E07\u0E32\u0E19\u0E07\u0E48\u0E32\u0E22 \u0E41\u0E25\u0E30\u0E17\u0E33\u0E07\u0E32\
  \u0E19\u0E44\u0E14\u0E49\u0E14\u0E35 1. \u0E40\u0E1E\u0E34\u0E48\u0E21 HTTPoison\
  \ \u0E43\u0E19\u0E44\u0E1F\u0E25\u0E4C `mix.exs` \u0E02\u0E2D\u0E07\u0E42\u0E1B\u0E23\
  \u0E40\u0E08\u0E04."
lastmod: '2024-03-17T21:57:55.847448-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 `HTTPoison`\
  \ \u0E02\u0E2D\u0E07 Elixir \u0E21\u0E31\u0E19\u0E40\u0E23\u0E35\u0E22\u0E1A\u0E07\
  \u0E48\u0E32\u0E22 \u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E07\u0E48\u0E32\u0E22\
  \ \u0E41\u0E25\u0E30\u0E17\u0E33\u0E07\u0E32\u0E19\u0E44\u0E14\u0E49\u0E14\u0E35\
  \n\n1."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## วิธีการ:
ใช้ไลบรารี `HTTPoison` ของ Elixir มันเรียบง่าย ใช้งานง่าย และทำงานได้ดี

1. เพิ่ม HTTPoison ในไฟล์ `mix.exs` ของโปรเจค:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

2. รันคำสั่ง `mix deps.get` ในเทอร์มินัลเพื่อดึง dependency ที่ต้องการ

3. ตอนนี้คุณพร้อมที่จะส่งคำขอ GET แล้ว:

```elixir
case HTTPoison.get("https://jsonplaceholder.typicode.com/posts/1") do
  {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
    IO.inspect(body) # คุณได้ข้อมูลของคุณแล้ว!
  {:error, %HTTPoison.Error{reason: reason}} ->
    IO.inspect(reason) # จัดการกับข้อผิดพลาด
end
```

ผลลัพธ์ตัวอย่าง: สตริง JSON ของข้อมูลโพสต์จาก API placeholder

## การทำความลึก
ในอดีต คุณอาจใช้ `:httpc` ที่มาพร้อมกับ Erlang/OTP หรือ `HTTPotion` ของ Elixir HTTPoison ได้รับความนิยมมากขึ้นในขณะนี้ ด้วย syntax ที่เรียบง่ายและสร้างบน Hackney ไคลเอ็นต์ HTTP ที่แข็งแกร่งสำหรับ Erlang

ทางเลือกอื่นๆ กับ HTTPoison ได้แก่ Tesla – ไคลเอ็นต์ HTTP ที่ยืดหยุ่นพร้อมการสนับสนุน middleware และ Mint – ไคลเอ็นต์ HTTP ระดับต่ำที่เหมาะสม

ในเชิงการเนินงาน ไลบรารีเหล่านี้จัดการกับการรวม connection, SSL และความคงอยู่ของการเชื่อมต่อ ซึ่งเป็นเรื่องยากแต่จำเป็นสำหรับคำขอ HTTP ที่มีประสิทธิภาพ พวกเขาเปรียบเสมือนบรรณารักษ์มิตรแท้ที่ดูแลรายละเอียดต่างๆ เพื่อให้คุณไม่ต้องคุ้ยหาด้วยตัวเอง

## ดูเพิ่มเติม
- [HTTPoison GitHub](https://github.com/edgurgel/httpoison) – สำหรับรายละเอียดและอัพเดททั้งหมด
- [HexDocs สำหรับ HTTPoison](https://hexdocs.pm/httpoison) – สถานที่สำหรับเอกสารอ้างอิงอย่างครบถ้วน
- [Elixir Forum](https://elixirforum.com) – เพื่อสนทนากับชุมชน
