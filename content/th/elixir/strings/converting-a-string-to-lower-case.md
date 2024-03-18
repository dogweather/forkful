---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:33.865971-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E43\u0E2B\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\
  \u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\u0E47\u0E01\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\
  \u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E17\u0E38\u0E01\u0E15\u0E31\u0E27\u0E2D\
  \u0E31\u0E01\u0E29\u0E23\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E2B\u0E49\
  \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E15\u0E31\u0E27\u0E1E\
  \u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\u0E47\u0E01 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E2D\u0E14\u0E04\
  \u0E25\u0E49\u0E2D\u0E07\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u2026"
lastmod: '2024-03-17T21:57:55.837459-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E43\u0E2B\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\
  \u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\u0E47\u0E01\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\
  \u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E17\u0E38\u0E01\u0E15\u0E31\u0E27\u0E2D\
  \u0E31\u0E01\u0E29\u0E23\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E2B\u0E49\
  \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E15\u0E31\u0E27\u0E1E\
  \u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\u0E47\u0E01 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E2D\u0E14\u0E04\
  \u0E25\u0E49\u0E2D\u0E07\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u2026"
title: "\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E47\u0E01"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การเปลี่ยนสตริงให้เป็นตัวพิมพ์เล็กคือการเปลี่ยนทุกตัวอักษรในสตริงให้เป็นรูปแบบตัวพิมพ์เล็ก โปรแกรมเมอร์ทำสิ่งนี้เพื่อความสอดคล้องในการเก็บข้อมูล การเปรียบเทียบ และการค้นหา

## วิธีการ:

Elixir ทำให้มันง่ายดาย เพียงใช้ฟังก์ชัน `String.downcase/1`:

```elixir
original = "LoReM IPSUM"
lowercased = String.downcase(original)

IO.puts original
IO.puts lowercased
```

ผลลัพธ์:

```
LoReM IPSUM
lorem ipsum
```

## อภิปรายเพิ่มเติม

การจัดการสตริงของ Elixir ระบุความรู้เกี่ยวกับ Unicode, ซึ่งมีความสำคัญมากสำหรับการเปลี่ยนเป็นตัวพิมพ์เล็กอย่างถูกต้องในอักษรและสคริปต์ที่หลากหลาย ในอดีต, การจัดการสตริงในภาษาโปรแกรมไม่ได้คำนึงถึงความซับซ้อนนี้เสมอไป

ก่อนมาถึงวิธีการปัจจุบันของ Elixir, ภาษาเก่าบางภาษาเสนอวิธีการที่ง่ายดายซึ่งอาจทำงานได้ดีสำหรับภาษาอังกฤษ แต่จะพลาดในภาษาเช่นตุรกี ซึ่งตัวอุปสรรค 'i' ในตัวพิมพ์ใหญ่ไม่ได้กลายเป็น 'I' แต่เป็น 'İ'

ในภายใน, Elixir ใช้การทำแผนที่ตัวพิมพ์ของ Unicode เพื่อทำสิ่งนี้ให้ถูกต้อง และมีทางเลือกอื่น ๆ; เช่น, `String.downcase/2` ช่วยให้คุณสามารถระบุ locale, ซึ่งมีประโยชน์สำหรับพฤติกรรมเพิ่มเติมที่เฉพาะเจาะจงของภาษา

```elixir
turkish = "GÖLCÜK"
String.downcase(turkish, :tr)
```

ผลลัพธ์:

```
gölcük
```

ในตัวอย่างด้านบน เห็นได้ชัดว่าตัวอักษร 'I' ได้รับการอนุรักษ์ไว้อย่างเหมาะสมตามกฎการใช้ตัวพิมพ์ของตุรกี

## ดูเพิ่มเติม

- เอกสารการใช้งานอย่างเป็นทางการของโมดูล `String` ของ Elixir: https://hexdocs.pm/elixir/String.html
- การทำแผนที่ตัวพิมพ์ของ Unicode: https://www.unicode.org/reports/tr21/tr21-5.html
- คู่มือแนะนำเบื้องต้นเกี่ยวกับ Unicode ใน Elixir: https://elixir-lang.org/blog/2017/01/05/elixir-and-unicode-part-1/
