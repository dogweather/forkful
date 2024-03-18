---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:23.498046-06:00
description: "\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E1B\u0E01\u0E15\u0E34 (regular\
  \ expressions \u0E2B\u0E23\u0E37\u0E2D regex) \u0E40\u0E1B\u0E47\u0E19\u0E25\u0E33\
  \u0E14\u0E31\u0E1A\u0E02\u0E2D\u0E07\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\
  \u0E1A\u0E1A\u0E43\u0E19\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32 \u0E42\u0E14\
  \u0E22\u0E2A\u0E48\u0E27\u0E19\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E0A\u0E49\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\
  \u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E2A\u0E15\u0E23\u0E34\u0E07\u2026"
lastmod: '2024-03-17T21:57:56.429491-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E1B\u0E01\u0E15\u0E34 (regular expressions\
  \ \u0E2B\u0E23\u0E37\u0E2D regex) \u0E40\u0E1B\u0E47\u0E19\u0E25\u0E33\u0E14\u0E31\
  \u0E1A\u0E02\u0E2D\u0E07\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E17\u0E35\
  \u0E48\u0E43\u0E0A\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\
  \u0E43\u0E19\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32 \u0E42\u0E14\u0E22\u0E2A\
  \u0E48\u0E27\u0E19\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E0A\u0E49\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E08\
  \u0E31\u0E14\u0E01\u0E32\u0E23\u0E2A\u0E15\u0E23\u0E34\u0E07\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E23\u0E01\u0E38\u0E25\u0E32\u0E23\
  \u0E4C\u0E40\u0E2D\u0E47\u0E01\u0E40\u0E1E\u0E23\u0E2A\u0E0A\u0E31\u0E19"
---

{{< edit_this_page >}}

## อะไรและทำไม?

นิพจน์ปกติ (regular expressions หรือ regex) เป็นลำดับของตัวอักษรที่ใช้เป็นรูปแบบในการค้นหา โดยส่วนใหญ่ใช้สำหรับการค้นหาและจัดการสตริง โปรแกรมเมอร์ใช้ประโยชน์จาก regex ใน PowerShell เพื่องานเช่นการตรวจสอบข้อมูล, การแยกข้อมูล, และการเปลี่ยนแปลงข้อมูล เนื่องจากมีประสิทธิภาพและความยืดหยุ่นในการจัดการกับรูปแบบที่ซับซ้อน

## วิธีใช้:

ใน PowerShell, คุณสามารถใช้ operators `-match`, `-replace`, และ `-split`, รวมถึงอื่นๆ ในการดำเนินการกับ regular expressions ได้ ลองดูตัวอย่างเหล่านี้:

### การใช้ `-match` เพื่อตรวจสอบว่าสตริงตรงกับรูปแบบหรือไม่
Operator นี้จะคืนค่า `$true` หากพบรูปแบบนั้นภายในสตริง และ `$false` หากไม่พบ

```powershell
"hello world" -match "\w+orld"
# ผลลัพธ์: True
```

### การดึงค่าที่ตรงกัน
คุณสามารถดึงค่าที่ตรงกันโดยการเข้าถึงตัวแปรอัตโนมัติ `$matches`.

```powershell
if ("I have 100 apples" -match "\d+") {
    "พบตัวเลข: " + $matches[0]
}
# ผลลัพธ์: พบตัวเลข: 100
```

### การใช้ `-replace` สำหรับการแทนที่
Operator `-replace` จะแทนที่ทุกครั้งที่พบรูปแบบด้วยสตริงที่ระบุ

```powershell
"foo bar baz" -replace "ba[rz]", "qux"
# ผลลัพธ์: foo qux qux
```

### การแยกสตริงด้วย `-split`
แยกสตริงออกเป็นอาร์เรย์ของสตริงย่อยตามรูปแบบ regex

```powershell
"The quick-brown_fox jumps" -split "[-_ ]"
# ผลลัพธ์: The quick brown fox jumps
```

### การจับคู่รูปแบบขั้นสูง
PowerShell ยังรองรับการดำเนินการ regex ที่ซับซ้อนมากขึ้นผ่านคลาส `[regex]` ที่ให้คุณเข้าถึงเมธอดอย่าง `Matches()`, `Replace()`, และ `Split()`.

```powershell
[regex]::Matches("June 24, August 9, Dec 12", "\b[A-Za-z]+\b").Value
# ผลลัพธ์: June August Dec

[regex]::Replace("100,000", "\B(?=(?:\d{3})+(?!\d))", ",")
# ผลลัพธ์: 100,000

[regex]::Split("one,two;three four", ",|;| ")
# ผลลัพธ์: one two three four
```

ตัวอย่างเหล่านี้แสดงถึงพลังและความหลากหลายของ regular expressions ใน PowerShell สำหรับการจัดการข้อมูลและการจับคู่รูปแบบ โดยการใช้ประโยชน์จาก regex, โปรแกรมเมอร์สามารถดำเนินการตรวจสอบข้อความที่ซับซ้อนได้อย่างมีประสิทธิภาพ
