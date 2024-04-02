---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:23.894943-06:00
description: "YAML \u0E2B\u0E23\u0E37\u0E2D YAML Ain't Markup Language \u0E40\u0E1B\
  \u0E47\u0E19\u0E20\u0E32\u0E29\u0E32\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\
  \u0E22\u0E25\u0E44\u0E25\u0E2A\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\
  \u0E48\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\
  \u0E29\u0E22\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\
  \u0E4C\u0E21\u0E31\u0E01\u0E43\u0E0A\u0E49\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C\u0E04\u0E2D\u0E19\u0E1F\u0E34\u0E01\u0E41\u0E25\u0E30\u0E01\u0E32\
  \u0E23\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E23\u0E30\u0E2B\u0E27\
  \u0E48\u0E32\u0E07\u0E20\u0E32\u0E29\u0E32\u0E23\u0E48\u0E27\u0E21\u0E01\u0E31\u0E19\
  \u2026"
lastmod: '2024-03-17T21:57:56.460909-06:00'
model: gpt-4-0125-preview
summary: "YAML \u0E2B\u0E23\u0E37\u0E2D YAML Ain't Markup Language \u0E40\u0E1B\u0E47\
  \u0E19\u0E20\u0E32\u0E29\u0E32\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\
  \u0E25\u0E44\u0E25\u0E2A\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\
  \u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\
  \u0E22\u0E4C \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E21\u0E31\u0E01\u0E43\u0E0A\u0E49\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E44\u0E1F\
  \u0E25\u0E4C\u0E04\u0E2D\u0E19\u0E1F\u0E34\u0E01\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\
  \u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E23\u0E30\u0E2B\u0E27\u0E48\
  \u0E32\u0E07\u0E20\u0E32\u0E29\u0E32\u0E23\u0E48\u0E27\u0E21\u0E01\u0E31\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
weight: 41
---

## อะไร & ทำไม?
YAML หรือ YAML Ain't Markup Language เป็นภาษาการซีเรียลไลส์ข้อมูลที่อ่านได้โดยมนุษย์ โปรแกรมเมอร์มักใช้สำหรับไฟล์คอนฟิกและการส่งข้อมูลระหว่างภาษาร่วมกัน ความเรียบง่ายและความสามารถในการอ่านได้ง่ายทำให้มันได้รับความนิยมอย่างมากสำหรับงานที่เกี่ยวข้องกับการตั้งค่าสภาพแวดล้อม แอปพลิเคชั่น หรือบริการที่การกำหนดค่ามีความสำคัญและควรเข้าใจและแก้ไขได้ง่าย

## วิธีการ:
PowerShell โดยค่าเริ่มต้นไม่มี cmdlet ในตัวสำหรับการแยกวิเคราะห์ YAML แต่สามารถใช้งานร่วมกับ YAML ได้อย่างไม่มีปัญหาเมื่อคุณใช้โมดูล `powershell-yaml` หรือแปลง YAML เป็นอ็อบเจกต์ของ PowerShell โดยใช้ `ConvertFrom-Json` ร่วมกับเครื่องมือเช่น `yq`

### การใช้โมดูล `powershell-yaml`:
ก่อนอื่น ติดตั้งโมดูล:
```PowerShell
Install-Module -Name powershell-yaml
```

เพื่ออ่านไฟล์ YAML:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

การเขียนอ็อบเจกต์ PowerShell ลงในไฟล์ YAML:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

ตัวอย่าง `output.yml`:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### การแยกวิเคราะห์ YAML ด้วย `yq` และ `ConvertFrom-Json`:
วิธีการอื่นๆ ใช้ `yq`, เครื่องมือประมวลผล YAML ที่เบาและพกพาได้  `yq` สามารถแปลง YAML เป็น JSON ซึ่ง PowerShell สามารถวิเคราะห์ได้โดยธรรมชาติ

ก่อนอื่น ตรวจสอบให้แน่ใจว่า `yq` ได้ถูกติดตั้งบนระบบของคุณ
จากนั้นรัน:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

วิธีนี้เป็นประโยชน์โดยเฉพาะสำหรับผู้ใช้ที่ทำงานในสภาพแวดล้อมข้ามแพลตฟอร์มหรือต้องการใช้ JSON ภายใน PowerShell.
