---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:38.304428-06:00
description: "\u0E08\u0E33\u0E19\u0E27\u0E19\u0E40\u0E0A\u0E34\u0E07\u0E0B\u0E49\u0E2D\
  \u0E19\u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22\u0E2A\u0E48\u0E27\
  \u0E19\u0E08\u0E23\u0E34\u0E07\u0E41\u0E25\u0E30\u0E2A\u0E48\u0E27\u0E19\u0E08\u0E34\
  \u0E19\u0E15\u0E20\u0E32\u0E1E \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E1E\u0E27\u0E01\u0E21\u0E31\u0E19\u0E43\u0E19\
  \u0E2A\u0E32\u0E02\u0E32\u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\
  \u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2A\u0E31\u0E0D\u0E0D\u0E32\u0E13, \u0E01\u0E25\
  \u0E28\u0E32\u0E2A\u0E15\u0E23\u0E4C\u0E04\u0E27\u0E2D\u0E19\u0E15\u0E31\u0E21,\
  \ \u0E41\u0E25\u0E30\u0E40\u0E21\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E04\u0E33\
  \u0E19\u0E27\u0E13\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u2026"
lastmod: '2024-03-17T21:57:56.389366-06:00'
model: gpt-4-0125-preview
summary: "\u0E08\u0E33\u0E19\u0E27\u0E19\u0E40\u0E0A\u0E34\u0E07\u0E0B\u0E49\u0E2D\
  \u0E19\u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22\u0E2A\u0E48\u0E27\
  \u0E19\u0E08\u0E23\u0E34\u0E07\u0E41\u0E25\u0E30\u0E2A\u0E48\u0E27\u0E19\u0E08\u0E34\
  \u0E19\u0E15\u0E20\u0E32\u0E1E \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E1E\u0E27\u0E01\u0E21\u0E31\u0E19\u0E43\u0E19\
  \u0E2A\u0E32\u0E02\u0E32\u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\
  \u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2A\u0E31\u0E0D\u0E0D\u0E32\u0E13, \u0E01\u0E25\
  \u0E28\u0E32\u0E2A\u0E15\u0E23\u0E4C\u0E04\u0E27\u0E2D\u0E19\u0E15\u0E31\u0E21,\
  \ \u0E41\u0E25\u0E30\u0E40\u0E21\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E04\u0E33\
  \u0E19\u0E27\u0E13\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23  \u0E40\u0E1E\u0E23\
  \u0E32\u0E30\u0E08\u0E33\u0E19\u0E27\u0E19\u0E08\u0E23\u0E34\u0E07\u0E18\u0E23\u0E23\
  \u0E21\u0E14\u0E32\u0E44\u0E21\u0E48\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E15\u0E2D\
  \u0E1A\u0E42\u0E08\u0E17\u0E22\u0E4C\u0E44\u0E14\u0E49."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19"
weight: 14
---

## วิธีการ:
Bash ไม่รองรับจำนวนเชิงซ้อนตามธรรมชาติ คุณมักจะใช้เครื่องมือภายนอกเช่น `bc` พร้อมกับตัวเลือก `-l` นี่คือวิธีที่คุณสามารถคำนวณจำนวนเชิงซ้อนใน bash:

```bash
echo "sqrt(-1)" | bc -l
```

ผลลัพธ์:
```bash
j
```

การคูณ:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

ผลลัพธ์:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## ขุดลึก
จำนวนเชิงซ้อนมีมาตั้งแต่ศตวรรษที่ 16 แต่ภาษาสคริปต์เช่น Bash ไม่ถูกออกแบบมาสำหรับการคำนวณทางคณิตศาสตร์เช่นจำนวนเชิงซ้อนโดยตรง นั่นคือเหตุผลที่ `bc` หรือเครื่องมืออื่นเช่น `awk` มักถูกนำมาใช้ ภาษาเลือกอื่นๆ สำหรับการทำงานกับจำนวนเชิงซ้อน ได้แก่ Python ที่มีโมดูล `cmath` และ MATLAB ซึ่งทั้งคู่ถูกสร้างมาสำหรับฟังก์ชั่นทางคณิตศาสตร์ที่ซับซ้อนมากขึ้น สำหรับ Bash ก็เกี่ยวกับการใช้เครื่องมือ - `bc` ใช้ตัวอักษร 'i' ตัวพิมพ์เล็กในการแทนหน่วยจินตภาพ และรองรับการดำเนินการพื้นฐานเช่น การบวก, การลบ, การคูณ และการหาร

## ดูเพิ่มเติม
- คู่มือ `bc`: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (ทางเลือกสำหรับ MATLAB): https://www.gnu.org/software/octave/
- โมดูล `cmath` ของ Python: https://docs.python.org/3/library/cmath.html
