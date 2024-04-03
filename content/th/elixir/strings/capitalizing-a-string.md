---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:33.956051-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Elixir \u0E43\u0E2B\u0E49\
  \u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E07\u0E48\u0E32\u0E22\
  \u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\
  \u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\
  \u0E43\u0E2B\u0E0D\u0E48\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\
  \u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E21\u0E35\u0E43\u0E2B\u0E49\u0E40\u0E25\
  \u0E22\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E1E\u0E36\u0E48\
  \u0E07\u0E1E\u0E32\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E02\u0E2D\u0E07\u0E1A\
  \u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21 \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\
  \u0E46."
lastmod: '2024-03-17T21:57:55.833662-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u0E43\u0E2B\u0E49\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\
  \u0E35\u0E48\u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E17\u0E33\
  \u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\
  \u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E42\u0E14\u0E22\u0E43\
  \u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E21\
  \u0E35\u0E43\u0E2B\u0E49\u0E40\u0E25\u0E22\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\
  \u0E49\u0E2D\u0E07\u0E1E\u0E36\u0E48\u0E07\u0E1E\u0E32\u0E44\u0E25\u0E1A\u0E23\u0E32\
  \u0E23\u0E35\u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\
  \u0E32\u0E21 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\
  \u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E46."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E15\u0E31\u0E27\u0E2D\u0E31\
  \u0E01\u0E29\u0E23\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 2
---

## วิธีการ:
Elixir ให้วิธีการที่ง่ายต่อการทำให้สตริงเป็นตัวพิมพ์ใหญ่โดยใช้ฟังก์ชันที่มีให้เลยโดยไม่ต้องพึ่งพาไลบรารีของบุคคลที่สาม นี่คือตัวอย่างง่ายๆ:

```elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

ผลลัพธ์:

```
Elixir programming
```

สำหรับกรณีที่ต้องการควบคุมหรือตรรกะของการทำให้เป็นตัวพิมพ์ใหญ่ที่ซับซ้อนมากขึ้น คุณอาจผสมผสานฟังก์ชัน String ต่าง ๆ ใช้งานร่วมกัน ตัวอย่างเช่น หากคุณต้องการทำให้ทุกคำในประโยคเป็นตัวพิมพ์ใหญ่ คุณสามารถแยกประโยคออกเป็นคำ ทำให้แต่ละคำเป็นตัวพิมพ์ใหญ่ แล้วจึงนำมาเชื่อมต่อกัน:

```elixir
sentence = "elixir is fun"
capitalized_sentence = sentence 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts capitalized_sentence
```

ผลลัพธ์:

```
Elixir Is Fun
```

ในขณะที่ไลบรารีมาตรฐานของ Elixir ครอบคลุมความต้องการส่วนใหญ่ สำหรับการจัดการข้อความที่ซับซ้อนขึ้น รวมถึงการทำให้สตริงเป็นตัวพิมพ์ใหญ่ที่ก้าวหน้ายิ่งขึ้น คุณอาจสำรวจไลบรารีของบุคคลที่สาม เช่น Cldr สำหรับการสากลนาโนมัติ ซึ่งสามารถนำเสนอพฤติกรรมการทำให้เป็นตัวพิมพ์ใหญ่ตามสถานที่เฉพาะ.
