---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:45.560156-06:00
description: "\u0E08\u0E33\u0E19\u0E27\u0E19\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19\
  \ \u0E04\u0E37\u0E2D\u0E08\u0E33\u0E19\u0E27\u0E19\u0E17\u0E35\u0E48\u0E21\u0E35\
  \u0E2A\u0E48\u0E27\u0E19\u0E08\u0E23\u0E34\u0E07\u0E41\u0E25\u0E30\u0E2A\u0E48\u0E27\
  \u0E19\u0E08\u0E34\u0E19\u0E15\u0E20\u0E32\u0E1E (\u0E40\u0E0A\u0E48\u0E19 3 + 4i)\
  \ \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E2A\u0E33\u0E04\u0E31\u0E0D\
  \u0E43\u0E19\u0E2A\u0E32\u0E02\u0E32\u0E40\u0E0A\u0E48\u0E19 \u0E27\u0E34\u0E28\u0E27\
  \u0E01\u0E23\u0E23\u0E21, \u0E1F\u0E34\u0E2A\u0E34\u0E01\u0E2A\u0E4C \u0E41\u0E25\
  \u0E30\u0E27\u0E34\u0E17\u0E22\u0E32\u0E28\u0E32\u0E2A\u0E15\u0E23\u0E4C\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u2026"
lastmod: '2024-03-17T21:57:56.433180-06:00'
model: gpt-4-0125-preview
summary: "\u0E08\u0E33\u0E19\u0E27\u0E19\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19\
  \ \u0E04\u0E37\u0E2D\u0E08\u0E33\u0E19\u0E27\u0E19\u0E17\u0E35\u0E48\u0E21\u0E35\
  \u0E2A\u0E48\u0E27\u0E19\u0E08\u0E23\u0E34\u0E07\u0E41\u0E25\u0E30\u0E2A\u0E48\u0E27\
  \u0E19\u0E08\u0E34\u0E19\u0E15\u0E20\u0E32\u0E1E (\u0E40\u0E0A\u0E48\u0E19 3 + 4i)\
  \ \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E2A\u0E33\u0E04\u0E31\u0E0D\
  \u0E43\u0E19\u0E2A\u0E32\u0E02\u0E32\u0E40\u0E0A\u0E48\u0E19 \u0E27\u0E34\u0E28\u0E27\
  \u0E01\u0E23\u0E23\u0E21, \u0E1F\u0E34\u0E2A\u0E34\u0E01\u0E2A\u0E4C \u0E41\u0E25\
  \u0E30\u0E27\u0E34\u0E17\u0E22\u0E32\u0E28\u0E32\u0E2A\u0E15\u0E23\u0E4C\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E08\
  \u0E33\u0E25\u0E2D\u0E07, \u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\
  \u0E2A\u0E31\u0E0D\u0E0D\u0E32\u0E13 \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E41\u0E01\
  \u0E49\u0E1B\u0E31\u0E0D\u0E2B\u0E32\u0E04\u0E13\u0E34\u0E15\u0E28\u0E32\u0E2A\u0E15\
  \u0E23\u0E4C\u0E1B\u0E23\u0E30\u0E40\u0E20\u0E17\u0E40\u0E09\u0E1E\u0E32\u0E30."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19"
weight: 14
---

## วิธีการ:
PowerShell ไม่มีการสนับสนุนจำนวนซับซ้อนในตัว เพราะฉะนั้นคุณต้องสร้างโซลูชันของคุณเองหรือใช้ `System.Numerics.Complex` ของ .NET 

```PowerShell
# มาสร้างจำนวนซับซ้อนโดยใช้ .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# สร้างจำนวนซับซ้อน
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# บวกจำนวนซับซ้อนสองตัว
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# คูณจำนวนซับซ้อนสองตัว
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# แสดงผลลัพธ์
"Sum: $sum"
"Product: $product"
```
ผลลัพธ์:
```
Sum: (4, 6)
Product: (-5, 10)
```

## ความรู้เจาะลึก
จำนวนซับซ้อนถูกพัฒนาขึ้นในศตวรรษที่ 16 เพื่อแก้สมการที่ไม่มีโซลูชันในโลกของจำนวนจริง ตอนนี้พวกมันเป็นหัวมุมหลักของคณิตศาสตร์สมัยใหม่

การที่ PowerShell พึ่งพา .NET สำหรับการสนับสนุนจำนวนซับซ้อนหมายถึงประสิทธิภาพที่แข็งแกร่ง ตัวเลือกอื่น ๆ รวมถึงไลบรารีของบุคคลที่สามหรือภาษาโปรแกรมอื่น ๆ เช่น Python ซึ่งจำนวนซับซ้อนเป็นประเภทข้อมูลดั้งเดิม

## ดูเพิ่มเติม
- [โครงสร้าง System.Numerics.Complex](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [การคำนวณจำนวนซับซ้อนใน Python](https://docs.python.org/3/library/cmath.html)
