---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:44.210488-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Bash \u0E44\u0E21\u0E48\u0E21\
  \u0E35\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E48\u0E19\u0E17\u0E35\u0E48\u0E15\
  \u0E31\u0E49\u0E07\u0E44\u0E27\u0E49\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E40\u0E1B\u0E25\
  \u0E35\u0E48\u0E22\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E2B\u0E49\u0E40\u0E1B\
  \u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48\
  \u2026"
lastmod: '2024-03-17T21:57:56.379193-06:00'
model: gpt-4-0125-preview
summary: "Bash \u0E44\u0E21\u0E48\u0E21\u0E35\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\
  \u0E48\u0E19\u0E17\u0E35\u0E48\u0E15\u0E31\u0E49\u0E07\u0E44\u0E27\u0E49\u0E40\u0E09\
  \u0E1E\u0E32\u0E30\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\
  \u0E07\u0E32\u0E19\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E43\u0E2B\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\
  \u0E1E\u0E4C\u0E43\u0E2B\u0E0D\u0E48 \u0E41\u0E15\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E20\u0E32\u0E23\u0E01\u0E34\u0E08\u0E19\u0E35\
  \u0E49\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E01\u0E32\u0E23\u0E02\
  \u0E22\u0E32\u0E22\u0E1E\u0E32\u0E23\u0E32\u0E21\u0E34\u0E40\u0E15\u0E2D\u0E23\u0E4C\
  \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\u0E2D\
  \u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\u0E40\u0E0A\u0E48\u0E19 `awk` \u0E19\u0E35\u0E48\
  \u0E04\u0E37\u0E2D\u0E2A\u0E2D\u0E07\u0E2A\u0E32\u0E21\u0E27\u0E34\u0E18\u0E35\u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E43\u0E19 Bash \u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E15\u0E31\u0E27\u0E2D\u0E31\
  \u0E01\u0E29\u0E23\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 2
---

## วิธีการ:
Bash ไม่มีฟังก์ชั่นที่ตั้งไว้เฉพาะสำหรับการทำงานเปลี่ยนสตริงให้เป็นตัวพิมพ์ใหญ่ แต่คุณสามารถทำภารกิจนี้ได้โดยใช้การขยายพารามิเตอร์หรือเครื่องมือภายนอกเช่น `awk` นี่คือสองสามวิธีในการทำให้สตริงใน Bash เป็นตัวพิมพ์ใหญ่:

**โดยใช้การขยายพารามิเตอร์:**

วิธีนี้จะจัดการสตริงโดยตรงในเชลล์

```bash
str="hello world"
capitalized="${str^}"
echo "$capitalized"
```
ผลลัพธ์:
```
Hello world
```

**โดยใช้ `awk`:**

`awk` เป็นเครื่องมือประมวลผลข้อความที่มีประสิทธิภาพ ซึ่งมีอยู่ในระบบปฏิบัติการที่คล้าย Unix ส่วนใหญ่ สามารถใช้เพื่อทำให้สตริงเป็นตัวพิมพ์ใหญ่

```bash
str="hello world"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
ผลลัพธ์:
```
Hello world
```

**โดยใช้ `sed`:**

สำหรับวิธีการที่แบบดั้งเดิมยิ่งขึ้น `sed` สามารถใช้เพื่อทำให้ตัวอักษรแรกของสตริงเป็นตัวพิมพ์ใหญ่ อย่างไรก็ตาม วิธีนี้มีความซับซ้อนมากกว่าวิธีก่อนหน้า

```bash
str="hello world"
echo "$str" | sed 's/./\u&/'
```
ผลลัพธ์:
```
Hello world
```

ชิ้นส่วนของโค้ดเหล่านี้แสดงวิธีทำให้อักขระแรกของสตริงใน Bash เป็นตัวพิมพ์ใหญ่ ซึ่งเน้นถึงความยืดหยุ่นของการเขียนสคริปต์เชลล์เมื่อจัดการกับข้อความ
