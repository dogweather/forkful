---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:41.965004-06:00
description: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E15\u0E31\u0E27\
  \u0E41\u0E01\u0E49\u0E08\u0E38\u0E14\u0E1A\u0E01\u0E1E\u0E23\u0E48\u0E2D\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E08\
  \u0E38\u0E14\u0E2B\u0E22\u0E38\u0E14 (breakpoints), \u0E01\u0E32\u0E23\u0E40\u0E14\
  \u0E34\u0E19\u0E1C\u0E48\u0E32\u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\
  \u0E38\u0E13, \u0E01\u0E32\u0E23\u0E14\u0E39\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\
  , \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\
  \u0E2A\u0E16\u0E32\u0E19\u0E30\u0E02\u0E2D\u0E07\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E02\u0E13\u0E30\u0E17\u0E35\u0E48\u0E21\u0E31\u0E19\u0E01\u0E33\u0E25\u0E31\
  \u0E07\u0E17\u0E33\u0E07\u0E32\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.446019-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E15\u0E31\u0E27\
  \u0E41\u0E01\u0E49\u0E08\u0E38\u0E14\u0E1A\u0E01\u0E1E\u0E23\u0E48\u0E2D\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E08\
  \u0E38\u0E14\u0E2B\u0E22\u0E38\u0E14 (breakpoints), \u0E01\u0E32\u0E23\u0E40\u0E14\
  \u0E34\u0E19\u0E1C\u0E48\u0E32\u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\
  \u0E38\u0E13, \u0E01\u0E32\u0E23\u0E14\u0E39\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\
  , \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\
  \u0E2A\u0E16\u0E32\u0E19\u0E30\u0E02\u0E2D\u0E07\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E02\u0E13\u0E30\u0E17\u0E35\u0E48\u0E21\u0E31\u0E19\u0E01\u0E33\u0E25\u0E31\
  \u0E07\u0E17\u0E33\u0E07\u0E32\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
---

## วิธีการ:
ใน PowerShell, คุณสามารถดีบักสคริปต์โดยใช้ PowerShell Integrated Scripting Environment (ISE) ที่มาพร้อมกับระบบ หรือ Visual Studio Code (VS Code) พร้อมกับส่วนขยาย PowerShell นี่คือวิธีการใช้จุดหยุดในทั้งสองระบบ:

### PowerShell ISE:
```PowerShell
# ตั้งจุดหยุดที่บรรทัดที่กำหนด
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# รันสคริปต์ตามปกติ
.\MyScript.ps1

# เมื่อสคริปต์ถึงจุดหยุด, คุณสามารถตรวจสอบตัวแปร
$myVariable

# ดำเนินการต่อ
Continue
```

### Visual Studio Code:
```PowerShell
# เปิดสคริปต์ PowerShell ใน VS Code
# คลิกที่ซ้ายของหมายเลขบรรทัดเพื่อตั้งจุดหยุด
# เริ่มการดีบักโดยกด F5 หรือคลิก 'Start Debugging'

# VS Code จะหยุดการดำเนินการที่จุดหยุดของคุณ
# ใช้แผงดีบักเพื่อดูตัวแปร, ตรวจสอบ call stack, และควบคุมการไหลของการดำเนินการ
```

การดีบักในทั้งสองสภาพแวดล้อมช่วยให้คุณสามารถข้ามเข้าไป (F11), ข้ามออก (Shift+F11) ขณะดีบัก

## การเจาะลึก
ในอดีต, การดีบักใน PowerShell มักจะดูงอกง่อย; มันจำเป็นต้องมีบรรทัด `Write-Host` มากมายเพื่อแสดงผลสถานะตัวแปรหรือใช้วิธีลองผิดลองถูกแบบคลาสสิก ด้วยการมาถึงของ PowerShell ISE, และเมื่อเร็วๆ นี้, VS Code พร้อมกับคุณสมบัติดีบักที่มีประสิทธิภาพ, การดีบักของ PowerShell กลายเป็นเรื่องที่เกือบเท่ากับภาษาโปรแกรมมิ่งอื่นๆ 

ทางเลือกสำหรับเครื่องมือดีบักของ PowerShell ประกอบด้วยเครื่องมือของบริษัทอื่นๆ เช่น PowerGUI หรือการใช้งาน IDE ที่มีความเข้มแข็งเช่น Visual Studio พร้อมกับปลั๊กอิน PowerShell

เมื่อใช้งานตัวแก้จุดบกพร่อง, ควรพิจารณาขอบเขตของสคริปต์, โดยเฉพาะอย่างยิ่งเมื่อทำงานกับสคริปต์หรือโมดูลที่มาจากการเชื่อมโยงแบบจุดท (dot-sourced) จุดหยุดสามารถตั้งได้ตามเงื่อนไข, เปลี่ยนแปลงตามตัวแปร, หรือตามบรรทัด, ทำให้สามารถควบคุมได้อย่างแม่นยำในระหว่างการดีบัก

นอกจากนี้, ด้วยการเปลี่ยนไปใช้ PowerShell Core (cross-platform PowerShell), การดีบักส่วนใหญ่ได้ย้ายไปอยู่ในมือของ VS Code, ซึ่งให้ประสบการณ์ที่สอดคล้องกันบนแพลตฟอร์มต่างๆ

## ดูเพิ่มเติม
สำหรับข้อมูลเพิ่มเติมเกี่ยวกับการดีบักใน PowerShell:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
