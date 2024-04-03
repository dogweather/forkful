---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:30.435309-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E2A\u0E48\u0E27\u0E19\u0E22\
  \u0E48\u0E2D\u0E22\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\
  \u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E2A\u0E48\u0E27\u0E19\
  \u0E40\u0E09\u0E1E\u0E32\u0E30\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\
  \u0E34\u0E07 \u2014 \u0E04\u0E34\u0E14\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E15\u0E31\u0E14\u0E40\u0E28\u0E29\u0E40\u0E2A\u0E49\
  \u0E19\u0E14\u0E49\u0E32\u0E22\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E40\u0E2A\u0E37\
  \u0E49\u0E2D\u0E16\u0E31\u0E01 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E01\u0E32\u0E23\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E41\u0E22\u0E01, \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\
  \u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.384814-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E2A\u0E48\u0E27\u0E19\u0E22\
  \u0E48\u0E2D\u0E22\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\
  \u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E2A\u0E48\u0E27\u0E19\
  \u0E40\u0E09\u0E1E\u0E32\u0E30\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\
  \u0E34\u0E07 \u2014 \u0E04\u0E34\u0E14\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E15\u0E31\u0E14\u0E40\u0E28\u0E29\u0E40\u0E2A\u0E49\
  \u0E19\u0E14\u0E49\u0E32\u0E22\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E40\u0E2A\u0E37\
  \u0E49\u0E2D\u0E16\u0E31\u0E01 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E01\u0E32\u0E23\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E41\u0E22\u0E01, \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\
  \u0E4C \u0E2B\u0E23\u0E37\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E1D\u0E31\u0E07\u0E2D\u0E22\u0E39\u0E48\u0E43\
  \u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21."
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## วิธีการ:
นี่คือความรู้เกี่ยวกับการสกัดส่วนย่อยของสตริงใน Bash:

```Bash
# ใช้ ${string:start:length}
text="The quick brown fox"
substring=${text:4:5}
echo $substring  # แสดงผล 'quick'

# ความยาวเริ่มต้นคือส่วนที่เหลือของสตริง
substring=${text:16}
echo $substring  # แสดงผล 'fox'

# ดัชนีเริ่มต้นเป็นลบ (จากปลายสตริง)
substring=${text: -3}
echo $substring  # แสดงผล 'fox'
```

## ลงลึก
Bash เคยจัดการกับสตริงมาอย่างยาวนาน การสกัดส่วนย่อยของสตริงเป็นเทคนิคเก่าแก่ แต่ยังคงเป็นมือที่ยอดเยี่ยม ก่อนที่จะมีเครื่องมือที่ซับซ้อน, เราเพียงแค่มีการขยายพารามิเตอร์ – ไวยากรณ์ `${}` – และมันยืนหยัดผ่านการทดสอบของเวลา

มีทางเลือกอื่นหรือไม่? แน่นอน `awk`, `cut`, และ `grep` สามารถสับและหั่นสตริงได้ในแบบของตัวเอง แต่สำหรับงานที่รวดเร็ว, ไม่ต้องเพิ่มกระบวนการ, วิธีการในตัวของ Bash มีประสิทธิภาพ

ในเรื่องของการดำเนินการ, Bash จับส่วนย่อยของสตริงได้โดยไม่มีอุปสรรค ไม่สนใจว่าภายในสตริงของคุณมีอะไร: ข้อความ, ตัวเลข, อิโมจิยูนิคอร์น — อะไรก็ตาม แค่ให้จุดเริ่มต้นและสิ้นสุด, มันจะตัดชิ้นนั้นออกมาอย่างไม่ลังเล

## ดูเพิ่มเติม
ลงลึกลงไปอีกและตรวจสอบลิงค์เหล่านี้:

- คู่มือ Bash เกี่ยวกับการขยายพารามิเตอร์: `man bash` และค้นหาข้อมูล *Parameter Expansion*
- การลงลึกใน `awk` และ `grep`: [คู่มือ Awk](https://www.gnu.org/software/gawk/manual/) และ [คู่มือ Grep](https://www.gnu.org/software/grep/manual/grep.html)
- ภาพรวมที่กว้างขึ้นเกี่ยวกับการจัดการสตริงใน Bash: [คู่มือการจัดการสตริง Bash](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
