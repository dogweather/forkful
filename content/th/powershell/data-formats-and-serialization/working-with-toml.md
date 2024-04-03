---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:04.990563-06:00
description: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious,\
  \ Minimal Language \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E01\
  \u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E25\u0E44\u0E25\u0E0B\u0E4C\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E07\u0E48\
  \u0E32\u0E22\u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01\u0E21\u0E35\u0E0B\
  \u0E35\u0E41\u0E21\u0E19\u0E15\u0E34\u0E01\u0E2A\u0E4C\u0E17\u0E35\u0E48\u0E0A\u0E31\
  \u0E14\u0E40\u0E08\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.463685-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious, Minimal\
  \ Language \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E01\u0E32\
  \u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E25\u0E44\u0E25\u0E0B\u0E4C\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E07\u0E48\u0E32\
  \u0E22\u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01\u0E21\u0E35\u0E0B\u0E35\
  \u0E41\u0E21\u0E19\u0E15\u0E34\u0E01\u0E2A\u0E4C\u0E17\u0E35\u0E48\u0E0A\u0E31\u0E14\
  \u0E40\u0E08\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\
  \u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32 \u0E40\
  \u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01\u0E21\u0E31\u0E19\u0E21\u0E35\u0E04\
  \u0E27\u0E32\u0E21\u0E2A\u0E21\u0E14\u0E38\u0E25\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\
  \u0E07\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\
  \u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\
  \u0E22\u0E4C\u0E41\u0E25\u0E30\u0E40\u0E1B\u0E47\u0E19\u0E21\u0E34\u0E15\u0E23\u0E01\
  \u0E31\u0E1A\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E31\u0E01\u0E23."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## วิธีการ:
ใน PowerShell ไม่มี cmdlet อย่างเป็นธรรมชาติที่จะแยกส่วน TOML คุณจะใช้โมดูลหรือแปลง TOML เป็น JSON ด้วยเครื่องมือเช่น `toml-to-json` ถ้าคุณต้องการทำงานกับ PowerShell นี่คือวิธีทำด้วยโมดูลจินตนาการ  `PowerShellTOML`:

```PowerShell
# ขั้นแรก ติดตั้งโมดูล (จินตนาการเพื่อการสาธิต)
Install-Module PowerShellTOML

# นำเข้าไฟล์ TOML
$config = Import-TomlConfig -Path './config.toml'

# เข้าถึงค่า
Write-Output $config.database.server

# เนื้อหา TOML ตัวอย่างใน 'config.toml':
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# ตัวอย่างผลลัพธ์:
# 192.168.1.1
```

## ลงลึก
TOML ถูกสร้างโดย Tom Preston-Werner ผู้ร่วมก่อตั้ง GitHub เป็นทางเลือกที่ง่ายกว่า XML และ YAML สำหรับไฟล์การตั้งค่า รุ่นแรกปรากฏในปี 2013 TOML สามารถเปรียบเทียบกับ JSON แต่ออกแบบมาเพื่อให้เป็นมิตรกับมนุษย์มากขึ้น ทำให้เป็นตัวเลือกที่ดีสำหรับการตั้งค่าที่ดูแลโดยผู้คน ทางเลือกอื่น ๆ ประกอบด้วย YAML, JSON และ XML

ในแง่ของการดำเนินการ โมดูล PowerShell สำหรับ TOML จะเป็น wrapper รอบ ๆ ไลบรารี่ TOML ที่เขียนในภาษาที่มุ่งเน้นประสิทธิภาพมากขึ้นเช่น C#. PowerShell ไม่มีการสนับสนุนในตัวสำหรับ TOML ซึ่งเป็นเหตุผลว่าทำไมโมดูลดังกล่าวจึงเป็นสิ่งจำเป็นในการทำงานกับรูปแบบ TOML ได้อย่างสะดวก

## ดูเพิ่มเติม
- มาตรฐาน TOML: https://toml.io/en/
- พื้นที่เก็บของ GitHub สำหรับโมดูล `toml` ใน PowerShell (ถ้ามีในเวลาที่อ่าน): https://github.com/powershell/PowerShellTOML
- การแนะนำ TOML: https://github.com/toml-lang/toml
- การเปรียบเทียบรูปแบบการซีเรียลไลซ์ข้อมูล: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
