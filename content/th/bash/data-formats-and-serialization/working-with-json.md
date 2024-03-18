---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:33.780594-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON\
  \ \u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21 Bash \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\
  \u0E2A\u0E48\u0E27\u0E19 \u0E04\u0E31\u0E14\u0E25\u0E2D\u0E01 \u0E41\u0E25\u0E30\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 JSON \u0E42\
  \u0E14\u0E22\u0E15\u0E23\u0E07\u0E08\u0E32\u0E01 command line\u2026"
lastmod: '2024-03-17T21:57:56.418810-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON\
  \ \u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21 Bash \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\
  \u0E2A\u0E48\u0E27\u0E19 \u0E04\u0E31\u0E14\u0E25\u0E2D\u0E01 \u0E41\u0E25\u0E30\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 JSON \u0E42\
  \u0E14\u0E22\u0E15\u0E23\u0E07\u0E08\u0E32\u0E01 command line\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
---

{{< edit_this_page >}}

## คืออะไร และทำไม?
การทำงานกับ JSON ในการเขียนโปรแกรม Bash คือการแยกส่วน คัดลอก และจัดการข้อมูล JSON โดยตรงจาก command line โปรแกรมเมอร์มักทำเช่นนี้เพื่อผสานการทำงานของสคริปต์ shell กับเว็บ API และรูปแบบการแลกเปลี่ยนข้อมูลสมัยใหม่ เพื่อทำให้การเขียนสคริปต์ Bash มีพลังและความเกี่ยวข้องมากขช้นในระบบนิเวศที่ใช้ JSON อย่างหนัก

## วิธีการ:
Bash เองไม่มีความสามารถในการแยกส่วน JSON ที่ติดตั้งมาด้วย แต่ `jq` เป็นโปรแกรมแปรรูป JSON ใน command-line ที่มีประสิทธิภาพและสามารถช่วยเติมเต็มส่วนที่ขาดหายไปนี้ นี่คือวิธีใช้งาน:

**การอ่านไฟล์ JSON:**

ตัวอย่าง `data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

เพื่ออ่านและคัดลอกชื่อจากไฟล์ JSON:
```bash
jq '.name' data.json
```
ผลลัพธ์:
```
"Jane Doe"
```

**การเปลี่ยนแปลงข้อมูล JSON:**

เพื่ออัปเดตเมืองเป็น "Los Angeles" และเขียนกลับไปยังไฟล์:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**การแยกข้อมูล JSON จากตัวแปร:**

หากคุณมี JSON อยู่ในตัวแปร Bash, `jq` สามารถแปรรูปมันได้:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
ผลลัพธ์:
```
"John Doe"
```

**การทำงานกับอาร์เรย์:**

กำหนดอาร์เรย์ของสิ่งของใน JSON:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

เพื่อคัดลอกสิ่งของที่สอง (การนับเริ่มที่ 0):
```bash
jq '.items[1]' data.json
```
ผลลัพธ์:
```
"banana"
```

สำหรับการดำเนินการที่ซับซ้อนและการกรองข้อมูลเพิ่มเติม, `jq` มีคู่มือการใช้งานและบทช่วยสอนออนไลน์ที่ครอบคลุม ทำให้มันเป็นเครื่องมือที่หลากหลายสำหรับทุกความต้องการของคุณในการใช้ Bash/JSON.
