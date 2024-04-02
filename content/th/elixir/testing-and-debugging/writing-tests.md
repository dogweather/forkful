---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:10.591370-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E43\u0E19 Elixir \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\
  \u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E04\
  \u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E1E\u0E24\
  \u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E23\u0E31\u0E1A\u0E23\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E20\u0E32\
  \u0E1E\u2026"
lastmod: '2024-03-17T21:57:55.856326-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E43\u0E19 Elixir \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\
  \u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E04\
  \u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E1E\u0E24\
  \u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E23\u0E31\u0E1A\u0E23\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E20\u0E32\
  \u0E1E\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E01\u0E32\u0E23\u0E17\u0E14\
  \u0E2A\u0E2D\u0E1A"
weight: 36
---

## อะไร & ทำไม?
การเขียนทดสอบใน Elixir เกี่ยวข้องกับการสร้างสคริปต์อัตโนมัติเพื่อตรวจสอบพฤติกรรมของโค้ดของคุณ โปรแกรมเมอร์ทำเช่นนี้เพื่อรับรองคุณภาพ ป้องกันการเปลี่ยนแปลงย้อนกลับ และอำนวยความสะดวกในการรีแฟกเตอร์โค้ด ทำให้กระบวนการพัฒนาน่าเชื่อถือและมีประสิทธิภาพมากขึ้น

## วิธีการ:
Elixir ใช้ ExUnit เป็นเฟรมเวิร์คทดสอบภายในที่มีประสิทธิภาพสูงและใช้งานง่าย นี่คือตัวอย่างพื้นฐาน:

1. สร้างไฟล์ทดสอบใหม่ในไดเรกทอรี `test` ของโปรเจ็กต์ Elixir ของคุณ ตัวอย่างเช่น ถ้าคุณกำลังทดสอบโมดูลที่ชื่อ `MathOperations` ไฟล์ทดสอบของคุณอาจเป็น `test/math_operations_test.exs`

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # นี่เป็นกรณีทดสอบง่ายๆ เพื่อเช็กฟังก์ชันการบวก
  test "การบวกของสองตัวเลข" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

เพื่อรันทดสอบของคุณ ใช้คำสั่ง `mix test` ในเทอร์มินัลของคุณ ถ้าฟังก์ชัน `MathOperations.add/2` บวกสองตัวเลขได้ถูกต้อง คุณจะเห็นผลลัพธ์ที่คล้ายกับ:

```
..

เสร็จสิ้นใน 0.03 วินาที
1 การทดสอบ, 0 ความล้มเหลว
```

สำหรับการทดสอบที่เกี่ยวข้องกับบริการภายนอกหรือ API คุณอาจต้องการใช้ไลบรารีโม๊ค เช่น `mox` เพื่อหลีกเลี่ยงการเชื่อมต่อกับบริการจริง:

1. เพิ่ม `mox` ใน dependencies ของคุณใน `mix.exs`:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # อื่นๆ...
  ]
end
```

2. กำหนดโมดูลโม๊คใน test helper ของคุณ (`test/test_helper.exs`):

```elixir
Mox.defmock(HTTPClientMock, for: HTTPClientBehaviour)
```

3. ใช้โม็คในกรณีทดสอบของคุณ:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # สิ่งนี้บอก Mox ว่าให้ตรวจสอบว่าโม็คนี้ถูกเรียกตามที่คาดหวัง
  setup :verify_on_exit!

  test "รับข้อมูลจาก API" do
    # ตั้งค่าการตอบกลับโม๊ค
    expect(HTTPClientMock, :get, fn _url -> {:ok, "Mocked response"} end)
    
    assert SomeAPIClient.get_data() == "Mocked response"
  end
end
```

เมื่อรัน `mix test`, การตั้งค่านี้ช่วยให้คุณแยกการทดสอบหน่วยออกจากการพึ่งพาภายนอกจริงๆ โดยมุ่งเน้นไปที่พฤติกรรมของโค้ดของคุณเอง รูปแบบนี้รับรองว่าการทดสอบของคุณทำงานได้เร็วและยังคงน่าเชื่อถือ ไม่ว่าสถานะของบริการภายนอกหรือการเชื่อมต่ออินเทอร์เน็ตจะเป็นอย่างไร.
