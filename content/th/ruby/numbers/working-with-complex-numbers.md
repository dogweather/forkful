---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:43.882582-06:00
description: "\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\
  \u0E19\u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22\u0E2A\u0E48\u0E27\
  \u0E19\u0E08\u0E23\u0E34\u0E07\u0E41\u0E25\u0E30\u0E2A\u0E48\u0E27\u0E19\u0E08\u0E34\
  \u0E19\u0E15\u0E20\u0E32\u0E1E (\u0E40\u0E0A\u0E48\u0E19 3+4i) \u0E40\u0E1B\u0E47\
  \u0E19\u0E2A\u0E48\u0E27\u0E19\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E43\u0E19\u0E27\u0E34\
  \u0E28\u0E27\u0E01\u0E23\u0E23\u0E21\u0E41\u0E25\u0E30\u0E1F\u0E34\u0E2A\u0E34\u0E01\
  \u0E2A\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\
  \u0E43\u0E19\u0E01\u0E32\u0E23\u0E08\u0E33\u0E25\u0E2D\u0E07\u0E2A\u0E16\u0E32\u0E19\
  \u0E01\u0E32\u0E23\u0E13\u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.721308-06:00'
model: gpt-4-0125-preview
summary: "\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\
  \u0E19\u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22\u0E2A\u0E48\u0E27\
  \u0E19\u0E08\u0E23\u0E34\u0E07\u0E41\u0E25\u0E30\u0E2A\u0E48\u0E27\u0E19\u0E08\u0E34\
  \u0E19\u0E15\u0E20\u0E32\u0E1E (\u0E40\u0E0A\u0E48\u0E19 3+4i) \u0E40\u0E1B\u0E47\
  \u0E19\u0E2A\u0E48\u0E27\u0E19\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E43\u0E19\u0E27\u0E34\
  \u0E28\u0E27\u0E01\u0E23\u0E23\u0E21\u0E41\u0E25\u0E30\u0E1F\u0E34\u0E2A\u0E34\u0E01\
  \u0E2A\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\
  \u0E43\u0E19\u0E01\u0E32\u0E23\u0E08\u0E33\u0E25\u0E2D\u0E07\u0E2A\u0E16\u0E32\u0E19\
  \u0E01\u0E32\u0E23\u0E13\u0E4C\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19"
---

{{< edit_this_page >}}

## คำว่าอะไร & ทำไม?
ตัวเลขซับซ้อนประกอบด้วยส่วนจริงและส่วนจินตภาพ (เช่น 3+4i) เป็นส่วนสำคัญในวิศวกรรมและฟิสิกส์ โปรแกรมเมอร์ทำงานกับพวกเขาในการจำลองสถานการณ์ การประมวลผลสัญญาณ และการแก้สมการที่ไม่สามารถทำได้ด้วยตัวเลขจริงเพียงอย่างเดียว

## วิธีการ:
Ruby ทำให้การจัดการตัวเลขซับซ้อนเป็นเรื่องง่าย คุณสามารถสร้างและจัดการกับพวกเขาโดยใช้คลาส Complex:

```ruby
require 'complex'

# สร้างตัวเลขซับซ้อน
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# การดำเนินการพื้นฐาน
sum = c1 + c2               # => (5.0+9.0i)
difference = c1 - c2        # => (1.0-1.0i)
product = c1 * c2           # => (-14.0+23.0i)
quotient = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# ผกผัน, ขนาด และ ระยะ
conjugate = c1.conjugate    # => (3.0-4.0i)
magnitude = c1.abs          # => 5.0
phase = c1.phase            # Math.atan2(4, 3) => 0.9272952180016122 รัศมี

# วิธีการเฉพาะสำหรับตัวเลขซับซ้อน
polar = c1.polar            # => [5.0, 0.9272952180016122]
rectangular = c1.rect       # => [3.0, 4.0]
```

## ลงลึกยิ่งขึ้น
ตัวเลขซับซ้อนไม่ใช่เรื่องใหม่—พวกเขามีมาตั้งแต่ศตวรรษที่ 16 โดยการแก้สมการที่ไม่มีคำตอบจริง นอกเหนือจากเรื่องคณิตศาสตร์ คลาส Complex ของ Ruby ทำงานหนัก โดยมีโมดูล Math ให้การสนับสนุนสำหรับฟังก์ชันตรีโกณมิติและข้ามธรรมชาติ

ภาษาโปรแกรมก่อนหน้านี้ต้องการการจัดการส่วนจริงและส่วนจินตภาพด้วยตนเอง บางภาษา เช่น Fortran และ C++ มีห้องสมุดพิเศษสำหรับการคำนวณซับซ้อน

วิธีการของ Ruby ฝังการสนับสนุนตัวเลขซับซ้อนไว้ในไวยากรณ์ของมัน ช่วยให้คุณไม่ต้องกลับไปเริ่มต้นใหม่ อยู่เบื้องหลัง คลาส Complex จัดการกับคณิตศาสตร์ ในขณะที่ Ruby ดูแลการทำงานร่วมกันของอ็อบเจกต์

## ดูเพิ่มเติมที่
- เอกสาร Ruby เกี่ยวกับ Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- ทัศนะของ MathWorld เกี่ยวกับตัวเลขซับซ้อน: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- เกริ่นนำเกี่ยวกับตัวเลขซับซ้อนและทำไมพวกเขาถึงมีประโยชน์: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
