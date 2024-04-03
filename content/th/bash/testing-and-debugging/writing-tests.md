---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:52.929315-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: Bash \u0E44\u0E21\u0E48\u0E21\u0E35\
  \u0E01\u0E23\u0E2D\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E17\u0E35\
  \u0E48\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E36\u0E49\u0E19\u0E21\u0E32\u0E40\u0E2D\
  \u0E07\u0E43\u0E19\u0E15\u0E31\u0E27 \u0E41\u0E15\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E40\u0E02\u0E35\u0E22\u0E19\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E19\u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E07\u0E48\u0E32\u0E22\u0E46\u0E44\
  \u0E14\u0E49 \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E14\u0E2A\
  \u0E2D\u0E1A\u0E17\u0E35\u0E48\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19\u0E02\u0E36\
  \u0E49\u0E19 \u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\u0E2D\u0E02\u0E2D\
  \u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E40\u0E0A\
  \u0E48\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.400908-06:00'
model: gpt-4-0125-preview
summary: "Bash \u0E44\u0E21\u0E48\u0E21\u0E35\u0E01\u0E23\u0E2D\u0E1A\u0E01\u0E32\u0E23\
  \u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E17\u0E35\u0E48\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\
  \u0E36\u0E49\u0E19\u0E21\u0E32\u0E40\u0E2D\u0E07\u0E43\u0E19\u0E15\u0E31\u0E27 \u0E41\
  \u0E15\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E40\u0E02\u0E35\
  \u0E22\u0E19\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E07\u0E48\u0E32\u0E22\u0E46\u0E44\u0E14\u0E49 \u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E01\u0E32\u0E23\u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E17\u0E35\u0E48\u0E0B\u0E31\
  \u0E1A\u0E0B\u0E49\u0E2D\u0E19\u0E02\u0E36\u0E49\u0E19 \u0E40\u0E04\u0E23\u0E37\u0E48\
  \u0E2D\u0E07\u0E21\u0E37\u0E2D\u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\
  \u0E35\u0E48\u0E2A\u0E32\u0E21\u0E40\u0E0A\u0E48\u0E19 `bats-core` \u0E44\u0E14\u0E49\
  \u0E23\u0E31\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E19\u0E34\u0E22\u0E21\n\n#."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E01\u0E32\u0E23\u0E17\u0E14\
  \u0E2A\u0E2D\u0E1A"
weight: 36
---

## วิธีทำ:
Bash ไม่มีกรอบการทดสอบที่สร้างขึ้นมาเองในตัว แต่คุณสามารถเขียนฟังก์ชันทดสอบง่ายๆได้ สำหรับการทดสอบที่ซับซ้อนขึ้น เครื่องมือของบุคคลที่สามเช่น `bats-core` ได้รับความนิยม

### ตัวอย่างการทดสอบพื้นฐานใน Pure Bash:
```bash
function test_example_function {
  result=$(your_function 'test_input')
  expected_output="expected_output"
  
  if [[ "$result" == "$expected_output" ]]; then
    echo "ทดสอบผ่าน"
    return 0
  else
    echo "ทดสอบไม่ผ่าน. คาดหวัง '$expected_output', ได้รับ '$result'"
    return 1
  fi
}

# เรียกใช้ฟังก์ชันทดสอบ
test_example_function
```
ผลลัพธ์ตัวอย่าง:
```
ทดสอบผ่าน
```

### การใช้ `bats-core` สำหรับการทดสอบ:
เริ่มแรก, ติดตั้ง `bats-core` มักจะทำได้ผ่าน package manager หรือโดยการโคลน repository ของมัน

จากนั้น, เขียนการทดสอบของคุณในไฟล์ `.bats` ที่แยกจากกัน

```bash
# ไฟล์: example_function.bats

#!/usr/bin/env bats

@test "ทดสอบฟังก์ชันตัวอย่าง" {
  result="$(your_function 'test_input')"
  expected_output="expected_output"
  
  [ "$result" == "$expected_output" ]
}
```
เพื่อทำการทดสอบ, เพียงแค่เรียกใช้ไฟล์ `.bats`:
```bash
bats example_function.bats
```
ผลลัพธ์ตัวอย่าง:
```
 ✓ ทดสอบฟังก์ชันตัวอย่าง

1 การทดสอบ, 0 ความล้มเหลว
```

วิธีนี้ช่วยให้คุณสามารถรวมการทดสอบเข้ากับกระบวนการพัฒนาของคุณได้อย่างง่ายดาย ทำให้มั่นใจได้ในความเชื่อถือได้และความมั่นคงของสคริปต์ Bash ของคุณ
