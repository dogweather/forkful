---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:39.023282-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: #."
lastmod: '2024-03-17T21:57:56.461863-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

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
