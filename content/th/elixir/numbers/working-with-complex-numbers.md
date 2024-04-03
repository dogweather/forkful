---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:42.406604-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33: Elixir \u0E44\u0E21\u0E48\u0E21\u0E35\
  \u0E08\u0E33\u0E19\u0E27\u0E19\u0E40\u0E0A\u0E34\u0E07\u0E0B\u0E49\u0E2D\u0E19\u0E40\
  \u0E1B\u0E47\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E02\u0E2D\
  \u0E07\u0E21\u0E31\u0E19 \u0E14\u0E31\u0E07\u0E19\u0E31\u0E49\u0E19\u0E40\u0E23\u0E32\
  \u0E15\u0E49\u0E2D\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E40\u0E2D\u0E07\u0E2B\u0E23\
  \u0E37\u0E2D\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 \u0E40\u0E0A\
  \u0E48\u0E19 `ComplexNum` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E23\u0E47\u0E27 \u0E46 \u0E42\u0E14\u0E22\
  \u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35."
lastmod: '2024-03-17T21:57:55.844096-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u0E44\u0E21\u0E48\u0E21\u0E35\u0E08\u0E33\u0E19\u0E27\u0E19\u0E40\
  \u0E0A\u0E34\u0E07\u0E0B\u0E49\u0E2D\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E48\u0E27\
  \u0E19\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E02\u0E2D\u0E07\u0E21\u0E31\u0E19 \u0E14\u0E31\
  \u0E07\u0E19\u0E31\u0E49\u0E19\u0E40\u0E23\u0E32\u0E15\u0E49\u0E2D\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E40\u0E2D\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E43\u0E0A\u0E49\u0E44\
  \u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 \u0E40\u0E0A\u0E48\u0E19 `ComplexNum` \u0E19\
  \u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\
  \u0E23\u0E47\u0E27 \u0E46 \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\
  \u0E23\u0E32\u0E23\u0E35."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19"
weight: 14
---

## การทำ:
Elixir ไม่มีจำนวนเชิงซ้อนเป็นส่วนหนึ่งของมัน ดังนั้นเราต้องสร้างเองหรือใช้ไลบรารี เช่น `ComplexNum` นี่คือตัวอย่างเร็ว ๆ โดยใช้ไลบรารี:

```elixir
# สมมติว่าคุณได้ติดตั้ง ComplexNum แล้ว
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# สร้างจำนวนเชิงซ้อนและเพิ่มพวกมันเข้าด้วยกัน
c1 = {3, 4}   # หมายถึง 3 + 4i
c2 = {2, -3}  # หมายถึง 2 - 3i
ผลลัพธ์ = ComplexMath.add(c1, c2)
IO.puts "ผลลัพธ์คือ: #{inspect(ผลลัพธ์)}"
```

สิ่งนี้จะมีผลลัพธ์:
```
ผลลัพธ์คือ: {5, 1}
```

นั่นหมายความว่าผลรวมของ `3 + 4i` และ `2 - 3i` คือ `5 + 1i`.

## ทำความเข้าใจลึกซึ้ง
จำนวนเชิงซ้อนปรากฏขึ้นในประวัติศาสตร์เพราะจำนวนธรรมดาไม่สามารถจัดการกับรากที่สองของจำนวนลบได้ ไม่ได้ถูกใช้จริงจังจนกระทั่งศตวรรษที่ 17 ด้วยความช่วยเหลือของนักคณิตศาสตร์เช่น René Descartes และ Gerolamo Cardano

ใน Elixir, คุณมักจะใช้ tuples เช่น `{3, 4}` สำหรับจำนวนเชิงซ้อน หรือใช้ไลบรารีที่เฉพาะเจาะจงเพื่อหลีกเลี่ยงการสร้างวงล้อใหม่ ไลบรารีมักจะดีกว่า - พวกเขาจัดการกับสิ่งเช่นการคูณและการหารที่ซับซ้อนเนื่องจากหน่วยจินตภาพ 'i' (สำหรับความรู้: `i` ยกกำลังสอง เท่ากับ `-1`)

## ดูเพิ่มเติม
ตรวจสอบทรัพยากรเหล่านี้:
- [ไลบรารี ComplexNum](https://hex.pm/packages/complex_num) สำหรับผู้จัดการแพคเกจของ Elixir, Hex.
- [โรงเรียน Elixir](https://elixirschool.com/en/), สำหรับหัวข้อและแบบฝึกหัด Elixir ขั้นสูง
- [Erlang -- โมดูลคณิตศาสตร์](http://erlang.org/doc/man/math.html), ซึ่ง Elixir ใช้ภายใต้ฝาครอบ, สำหรับความต้องการคณิตศาสตร์อื่น ๆ
