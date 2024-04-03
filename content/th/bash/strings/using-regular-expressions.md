---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:24.579129-06:00
description: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Regular expressions (regex) \u0E43\
  \u0E19 Bash \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E04\u0E49\u0E19\u0E2B\u0E32, \u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23, \u0E41\u0E25\u0E30\u0E14\u0E39\u0E41\u0E25\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E41\u0E25\u0E30\u0E44\u0E1F\u0E25\u0E4C\u0E15\u0E32\u0E21\u0E23\u0E39\u0E1B\
  \u0E41\u0E1A\u0E1A\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E44\u0E14\u0E49 \u0E19\u0E31\u0E01\
  \u0E1E\u0E31\u0E12\u0E19\u0E32\u0E0B\u0E2D\u0E1F\u0E15\u0E4C\u0E41\u0E27\u0E23\u0E4C\
  \u0E43\u0E0A\u0E49 regex \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E40\
  \u0E0A\u0E48\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.385753-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Regular expressions (regex) \u0E43\u0E19\
  \ Bash \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E04\u0E49\u0E19\u0E2B\u0E32, \u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23, \u0E41\u0E25\u0E30\u0E14\u0E39\u0E41\u0E25\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E41\u0E25\u0E30\u0E44\u0E1F\u0E25\u0E4C\u0E15\u0E32\u0E21\u0E23\u0E39\u0E1B\u0E41\
  \u0E1A\u0E1A\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E44\u0E14\u0E49 \u0E19\u0E31\u0E01\u0E1E\
  \u0E31\u0E12\u0E19\u0E32\u0E0B\u0E2D\u0E1F\u0E15\u0E4C\u0E41\u0E27\u0E23\u0E4C\u0E43\
  \u0E0A\u0E49 regex \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E40\u0E0A\
  \u0E48\u0E19 \u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E01\u0E32\
  \u0E23\u0E1B\u0E49\u0E2D\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E01\u0E32\
  \u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E44\
  \u0E1F\u0E25\u0E4C log, \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 \u0E40\u0E1E\u0E23\u0E32\u0E30\u0E27\u0E48\u0E32\
  \u0E21\u0E31\u0E19\u0E43\u0E2B\u0E49\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E22\
  \u0E37\u0E14\u0E2B\u0E22\u0E38\u0E48\u0E19\u0E41\u0E25\u0E30\u0E17\u0E23\u0E07\u0E1E\
  \u0E25\u0E31\u0E07\u0E43\u0E19\u0E01\u0E32\u0E23\u0E23\u0E30\u0E1A\u0E38\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E04\u0E27\u0E32\u0E21\
  \u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E43\u0E19\u0E01\u0E32\u0E23\u0E1B\u0E23\
  \u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\
  \u0E35\u0E48\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E23\u0E01\u0E38\u0E25\u0E32\u0E23\
  \u0E4C\u0E40\u0E2D\u0E47\u0E01\u0E40\u0E1E\u0E23\u0E2A\u0E0A\u0E31\u0E19"
weight: 11
---

## อะไรและทำไม?

การใช้ Regular expressions (regex) ใน Bash ช่วยให้คุณสามารถค้นหา, จัดการ, และดูแลสตริงและไฟล์ตามรูปแบบเฉพาะได้ นักพัฒนาซอฟต์แวร์ใช้ regex สำหรับงานเช่น การตรวจสอบการป้อนข้อมูล, การแยกวิเคราะห์ไฟล์ log, และการสกัดข้อมูล เพราะว่ามันให้วิธีที่ยืดหยุ่นและทรงพลังในการระบุรูปแบบสำหรับความต้องการในการประมวลผลข้อความที่ซับซ้อน

## วิธีการ:

### การจับคู่รูปแบบพื้นฐาน
เพื่อค้นหาว่าสตริงตรงกับรูปแบบหรือไม่ คุณสามารถใช้ `grep`, ยูทิลิตี้บรรทัดคำสั่งสำหรับการค้นหาชุดข้อมูลข้อความธรรมดาสำหรับบรรทัดที่ตรงกับ regular expression:

```bash
echo "Hello, World!" | grep -o "World"
# ผลลัพธ์: World
```

### สกัดข้อมูลเฉพาะ
เพื่อสกัดส่วนของข้อมูลที่ตรงกับรูปแบบ regex ของคุณ คุณสามารถใช้ `-o` กับ `grep`:

```bash
echo "Error: File not found" | grep -oE "[A-Za-z]+:"
# ผลลัพธ์: Error:
```

### การใช้ Regex กับ `sed`
`sed` (stream editor) เป็นยูทิลิตี้ทรงพลังสำหรับการแยกวิเคราะห์และการเปลี่ยนแปลงข้อความ นี่คือวิธีการใช้ `sed` กับ regex เพื่อแทนที่ข้อความ:

```bash
echo "Bash is great" | sed -e 's/great/awesome/'
# ผลลัพธ์: Bash is awesome
```

### การจับคู่รูปแบบในการแสดงคำสั่งเงื่อนไข
Bash ยังรองรับ regex ในการแสดงคำสั่งเงื่อนไขโดยตรง:

```bash
[[ "https://example.com" =~ ^https?:// ]] && echo "URL is valid" || echo "URL is invalid"
# ผลลัพธ์: URL is valid
```

### การจับคู่รูปแบบขั้นสูงและการจัดการกับ `awk`
`awk` เป็นเครื่องมือประมวลผลข้อความอีกตัวที่รองรับการสกัดและการจัดการข้อมูลที่ซับซ้อนมากขึ้น มันสามารถช่วยได้เมื่อทำงานกับข้อมูลข้อความที่มีโครงสร้าง เช่น CSVs:

```bash
echo -e "ID,Name,Age\n1,John,22\n2,Jane,24" | awk -F, '$3 > 22 {print $2 " is older than 22."}'
# ผลลัพธ์: Jane is older than 22.
```

ในขณะที่ฟังก์ชัน regex ที่ในตัวของ Bash ครอบคลุมการใช้งานในหลาย ๆ กรณี สำหรับการดำเนินการ regex ขั้นสูงมาก ๆ คุณอาจพิจารณาใช้ Bash scripts ร่วมกับสคริปต์ `perl` หรือ `python` เนื่องจากภาษาเหล่านี้ให้ไลบรารี regex ที่ทรงพลัง (เช่น, `re` ใน Python) ตัวอย่างง่าย ๆ ด้วย Python:

```bash
echo "Capture this 123" | python3 -c "import sys; import re; print(re.search('(\d+)', sys.stdin.read()).group(0))"
# ผลลัพธ์: 123
```

การรวมภาษาโปรแกรมเหล่านี้เมื่อจำเป็นสามารถช่วยให้คุณใช้ประโยชน์จาก regex ได้อย่างเต็มที่ใน Bash scripts ของคุณ
