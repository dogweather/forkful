---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:00.148004-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON\
  \ \u0E43\u0E19 Fish Shell \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\
  \u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\
  \u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\
  \u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 JSON \u0E0B\u0E36\u0E48\u0E07\u0E40\
  \u0E1B\u0E47\u0E19\u0E07\u0E32\u0E19\u0E17\u0E31\u0E48\u0E27\u0E44\u0E1B\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E48\
  \u0E32\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19 \u0E01\u0E32\
  \u0E23\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A API\u2026"
lastmod: '2024-03-17T21:57:56.665622-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON\
  \ \u0E43\u0E19 Fish Shell \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\
  \u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\
  \u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\
  \u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 JSON \u0E0B\u0E36\u0E48\u0E07\u0E40\
  \u0E1B\u0E47\u0E19\u0E07\u0E32\u0E19\u0E17\u0E31\u0E48\u0E27\u0E44\u0E1B\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E48\
  \u0E32\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19 \u0E01\u0E32\
  \u0E23\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A API\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## อะไรและทำไม?

การทำงานกับ JSON ใน Fish Shell เกี่ยวข้องกับการแยกวิเคราะห์และการสร้างข้อมูล JSON ซึ่งเป็นงานทั่วไปสำหรับการกำหนดค่าแอปพลิเคชัน การโต้ตอบกับ API และการปรับปรุงกระบวนการทำงานใน command-line ด้วยความที่ JSON มีบทบาทสำคัญในการพัฒนาเว็บและแอปพลิเคชัน การเข้าใจในการจัดการกับ JSON โดยตรงใน shell สามารถเพิ่มประสิทธิภาพในการจัดการอัตโนมัติและข้อมูลสำหรับโปรแกรมเมอร์ได้อย่างมาก

## วิธีการ:

Fish Shell เองไม่มีเครื่องมือในตัวสำหรับการแยกวิเคราะห์และสร้าง JSON อย่างไรก็ตาม มันสามารถรวมง่ายๆ กับเครื่องมือของบุคคลที่สามเช่น `jq` สำหรับการประมวลผล JSON `jq` เป็นเครื่องมือประมวลผล JSON ที่มีประสิทธิภาพและหลากหลายซึ่งช่วยให้คุณสามารถตัด กรอง จับคู่ และแปลงข้อมูลที่มีโครงสร้างด้วยภาษาที่ง่ายและมีอรรถประโยชน์

### การแยกวิเคราะห์ JSON ด้วย jq
เพื่อแยกวิเคราะห์ไฟล์ JSON และสกัดข้อมูลโดยใช้ `jq`:

```fish
# สมมติว่าคุณมีไฟล์ JSON ชื่อ 'data.json' ด้วยเนื้อหา: {"name":"Fish Shell","version":"3.4.0"}
cat data.json | jq '.name'
# ตัวอย่างผลลัพธ์
"Fish Shell"
```

### การสร้าง JSON ด้วย jq
การสร้างเนื้อหา JSON จากตัวแปรหรือผลลัพธ์ของ shell:

```fish
# สร้างวัตถุ JSON จากตัวแปร
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# ตัวอย่างผลลัพธ์
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### การกรองคอลเลกชัน JSON
สมมติว่าเรามีอาร์เรย์ของวัตถุในไฟล์ชื่อ `versions.json`:
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
เพื่อกรองอาร์เรย์นี้สำหรับเฉพาะเวอร์ชันที่เสถียร:

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# ตัวอย่างผลลัพธ์
"3.1.2"
"3.4.0"
```

ตัวอย่างที่ให้ไว้สาธิตถึงประโยชน์ของการรวม `jq` กับ Fish Shell สำหรับการดำเนินการกับ JSON การใช้เครื่องมือเช่นนี้ช่วยให้ประสบการณ์ shell มีประสิทธิภาพมากขึ้น ทำให้เป็นสภาพแวดล้อมที่น่าทึ่งสำหรับการจัดการกับรูปแบบข้อมูลสมัยใหม่
