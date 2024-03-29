---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:39.023282-06:00
description: "\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E15\u0E31\u0E27\u0E02\u0E2D\u0E07\
  \ PowerShell \u0E01\u0E31\u0E1A JSON (JavaScript Object Notation) \u0E04\u0E37\u0E2D\
  \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E27\u0E34\
  \u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C (\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\
  \u0E19) \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07 (\u0E01\
  \u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19) \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \ JSON\u2026"
lastmod: '2024-03-17T21:57:56.461863-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E15\u0E31\u0E27\u0E02\u0E2D\u0E07\
  \ PowerShell \u0E01\u0E31\u0E1A JSON (JavaScript Object Notation) \u0E04\u0E37\u0E2D\
  \u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E27\u0E34\
  \u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C (\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\
  \u0E19) \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07 (\u0E01\
  \u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19) \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \ JSON\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การรวมตัวของ PowerShell กับ JSON (JavaScript Object Notation) คือเกี่ยวกับการวิเคราะห์ (การอ่าน) และการสร้าง (การเขียน) ข้อมูล JSON ซึ่งเป็นรูปแบบที่พบบ่อยสำหรับการแลกเปลี่ยนข้อมูลบนเว็บ นักพัฒนาทำงานกับ JSON เพื่อโต้ตอบกับ Web APIs, ไฟล์การกำหนดค่า หรือเพื่อให้การแลกเปลี่ยนข้อมูลระหว่างภาษาและแพลตฟอร์มที่แตกต่างกัน เนื่องจากมีลักษณะเบาและไม่ขึ้นกับภาษาใด

## วิธีการ:

### การวิเคราะห์ JSON

เพื่ออ่านหรือวิเคราะห์ JSON ใน PowerShell, คุณสามารถใช้ Cmdlet `ConvertFrom-Json` โดยมีสตริง JSON ที่ให้มา, Cmdlet นี้จะแปลงมันเป็นออบเจ็กต์ PowerShell

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

ตัวอย่างการแสดงผล:

```
John Doe
```

ตัวอย่างนี้แสดงวิธีการวิเคราะห์สตริง JSON ง่ายๆ เพื่อเข้าถึงคุณสมบัติของออบเจ็กต์ที่ได้รับ

### การสร้าง JSON

เพื่อสร้าง JSON จากออบเจ็กต์ PowerShell, คุณสามารถใช้ Cmdlet `ConvertTo-Json` ซึ่งเป็นประโยชน์สำหรับเตรียมข้อมูลที่จะส่งไปยังเว็บเซอร์วิสหรือบันทึกลงในไฟล์การกำหนดค่า

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

ตัวอย่างการแสดงผล:

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

ส่วนของโค้ดนี้สร้างออบเจ็กต์ PowerShell และจากนั้นแปลงมันเป็นสตริง JSON.
