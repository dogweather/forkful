---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:44.199624-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Fish Shell \u0E43\u0E0A\u0E49\
  \u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 `test` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\
  \u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E1B\u0E23\u0E30\u0E40\u0E20\u0E17\u0E44\u0E1F\
  \u0E25\u0E4C\u0E41\u0E25\u0E30\u0E25\u0E31\u0E01\u0E29\u0E13\u0E30\u0E15\u0E48\u0E32\
  \u0E07 \u0E46 \u0E23\u0E27\u0E21\u0E16\u0E36\u0E07\u0E27\u0E48\u0E32\u0E40\u0E1B\
  \u0E49\u0E32\u0E2B\u0E21\u0E32\u0E22\u0E40\u0E1B\u0E47\u0E19\u0E44\u0E14\u0E40\u0E23\
  \u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48\u2026"
lastmod: '2024-03-17T21:57:56.659063-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell \u0E43\u0E0A\u0E49\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 `test`\
  \ \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E1B\
  \u0E23\u0E30\u0E40\u0E20\u0E17\u0E44\u0E1F\u0E25\u0E4C\u0E41\u0E25\u0E30\u0E25\u0E31\
  \u0E01\u0E29\u0E13\u0E30\u0E15\u0E48\u0E32\u0E07 \u0E46 \u0E23\u0E27\u0E21\u0E16\
  \u0E36\u0E07\u0E27\u0E48\u0E32\u0E40\u0E1B\u0E49\u0E32\u0E2B\u0E21\u0E32\u0E22\u0E40\
  \u0E1B\u0E47\u0E19\u0E44\u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\
  \u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\
  \u0E48\u0E32\u0E21\u0E35\u0E44\u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\
  \u0E2D\u0E22\u0E39\u0E48\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48."
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
weight: 20
---

## วิธีการ:
Fish Shell ใช้คำสั่ง `test` เพื่อตรวจสอบประเภทไฟล์และลักษณะต่าง ๆ รวมถึงว่าเป้าหมายเป็นไดเร็กทอรีหรือไม่ นี่คือรูปแบบพื้นฐานสำหรับการตรวจสอบว่ามีไดเร็กทอรีอยู่หรือไม่:

```fish
if test -d /path/to/dir
    echo "Directory exists"
else
    echo "Directory does not exist"
end
```
ผลลัพธ์ตัวอย่าง:
```
Directory exists
```

เพื่อการดำเนินการกับไฟล์และไดเร็กทอรี่ที่สะดวกยิ่งขึ้น คุณอาจหันไปใช้เครื่องมือภายนอกเช่น `fd` แม้ว่าจะใช้สำหรับการค้นหาไฟล์และไดเร็กทอรีมากกว่าเพียงแค่ตรวจสอบการมีอยู่ อย่างไรก็ตาม การรวมมันกับสคริปต์ Fish สามารถให้ผลลัพธ์ที่มีประโยชน์ได้:

```fish
set dir "/path/to/search"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "Directory exists"
else
    echo "Directory does not exist"
end
```

ตัวอย่าง `fd` นี้ค้นหาไดเร็กทอรีที่ระดับความลึกที่กำหนด และ `grep` ตรวจสอบการตรงกัน เพิ่มความหลากหลายในการตรวจสอบอย่างประณีต อย่างไรก็ตาม สำหรับวัตถุประสงค์โดยตรงในการตรวจสอบการมีอยู่ การยึดถือคำสั่ง `test` ที่มีอยู่ในตัวของ Fish เป็นทั้งประหยัดและตรงไปตรงมา
