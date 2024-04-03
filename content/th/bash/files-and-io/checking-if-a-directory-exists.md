---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:45.220083-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E17\u0E35\u0E48\u0E2A\
  \u0E33\u0E04\u0E31\u0E0D\u0E17\u0E35\u0E48\u0E2A\u0E38\u0E14, Bash \u0E0A\u0E48\u0E27\
  \u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\
  \u0E01\u0E32\u0E23\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E02\u0E2D\u0E07\u0E44\u0E14\
  \u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\
  \u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E40\u0E07\u0E37\u0E48\u0E2D\u0E19\u0E44\u0E02\
  \u0E41\u0E25\u0E30\u0E15\u0E31\u0E27\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\
  \u0E23 `-d`\u2026"
lastmod: '2024-03-17T21:57:56.411666-06:00'
model: gpt-4-0125-preview
summary: "\u0E17\u0E35\u0E48\u0E2A\u0E33\u0E04\u0E31\u0E0D\u0E17\u0E35\u0E48\u0E2A\
  \u0E38\u0E14, Bash \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E15\
  \u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E01\u0E32\u0E23\u0E21\u0E35\u0E2D\u0E22\u0E39\
  \u0E48\u0E02\u0E2D\u0E07\u0E44\u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\
  \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E40\u0E07\
  \u0E37\u0E48\u0E2D\u0E19\u0E44\u0E02\u0E41\u0E25\u0E30\u0E15\u0E31\u0E27\u0E14\u0E33\
  \u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23 `-d` \u0E14\u0E49\u0E32\u0E19\u0E25\u0E48\
  \u0E32\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E17\u0E35\u0E48\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E40\u0E2B\u0E47\u0E19\
  \u0E27\u0E48\u0E32\u0E01\u0E32\u0E23\u0E17\u0E33\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\
  \u0E08\u0E2A\u0E2D\u0E1A\u0E19\u0E35\u0E49\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E44\u0E23."
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
weight: 20
---

## วิธีการ:
ที่สำคัญที่สุด, Bash ช่วยให้คุณตรวจสอบการมีอยู่ของไดเร็กทอรีโดยใช้คำสั่งเงื่อนไขและตัวดำเนินการ `-d` ด้านล่างเป็นตัวอย่างที่ช่วยให้เห็นว่าการทำการตรวจสอบนี้ใช้งานอย่างไร

```bash
if [ -d "/path/to/directory" ]; then
    echo "The directory exists."
else
    echo "The directory does not exist."
fi
```

ผลลัพธ์ตัวอย่าง (ถ้าไดเร็กทอรีมีอยู่):
```
The directory exists.
```

ผลลัพธ์ตัวอย่าง (ถ้าไดเร็กทอรีไม่มีอยู่):
```
The directory does not exist.
```

สำหรับสคริปต์ที่ซับซ้อนขึ้น, มันเป็นเรื่องปกติที่จะผสมผสานการตรวจสอบกับการดำเนินการอื่นๆ เช่น การสร้างไดเร็กทอรีหากมันไม่มีอยู่:

```bash
DIR="/path/to/directory"
if [ -d "$DIR" ]; then
    echo "$DIR exists."
else
    echo "$DIR does not exist. Creating now..."
    mkdir -p "$DIR"
    echo "$DIR created."
fi
```

ผลลัพธ์ตัวอย่าง (ถ้าไดเร็กทอรีไม่มีอยู่แล้วจึงถูกสร้างขึ้น):
```
/path/to/directory does not exist. Creating now...
/path/to/directory created.
```

ถึงแม้ว่า Bash เองจะมีเครื่องมือที่แข็งแกร่งสำหรับการตรวจสอบเช่นนี้ แต่ก็ไม่มีไลบรารีของบุคคลที่สามที่ได้รับความนิยมโดยเฉพาะสำหรับงานนี้ เนื่องจากคำสั่ง Bash พื้นฐานมีความสามารถเพียงพอและมีประสิทธิภาพสำหรับการตรวจสอบการมีอยู่ของไดเร็กทอรี
