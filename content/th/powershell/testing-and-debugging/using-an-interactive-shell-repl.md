---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:38.150232-06:00
description: "\u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E2B\u0E23\u0E37\
  \u0E2D Read-Eval-Print Loop (REPL) \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\
  \u0E38\u0E13\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 PowerShell\
  \ \u0E41\u0E25\u0E30\u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E1C\u0E25\u0E15\u0E2D\
  \u0E1A\u0E23\u0E31\u0E1A\u0E17\u0E31\u0E19\u0E17\u0E35 \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E17\u0E14\u0E2A\u0E2D\u0E1A snippets\u2026"
lastmod: '2024-03-17T21:57:56.443254-06:00'
model: gpt-4-0125-preview
summary: "\u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E2B\u0E23\u0E37\
  \u0E2D Read-Eval-Print Loop (REPL) \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\
  \u0E38\u0E13\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 PowerShell\
  \ \u0E41\u0E25\u0E30\u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E1C\u0E25\u0E15\u0E2D\
  \u0E1A\u0E23\u0E31\u0E1A\u0E17\u0E31\u0E19\u0E17\u0E35 \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E17\u0E14\u0E2A\u0E2D\u0E1A snippets \u0E02\u0E2D\u0E07\
  \u0E42\u0E04\u0E49\u0E14\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\
  \u0E47\u0E27, \u0E01\u0E32\u0E23\u0E14\u0E35\u0E1A\u0E31\u0E01, \u0E2B\u0E23\u0E37\
  \u0E2D\u0E40\u0E23\u0E35\u0E22\u0E19\u0E23\u0E39\u0E49\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E43\u0E2B\u0E21\u0E48\u0E46 \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\
  \u0E2D\u0E07\u0E40\u0E02\u0E35\u0E22\u0E19\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\
  \u0E40\u0E15\u0E47\u0E21\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
weight: 34
---

## วิธีการ:
เปิด PowerShell และคุณจะอยู่ใน REPL ลองใช้ Cmdlet `Get-Date`:

```PowerShell
PS > Get-Date
```

คุณควรจะเห็นผลลัพธ์วันที่และเวลาปัจจุบัน:

```PowerShell
วันพุธที่ 31 มีนาคม 2023 12:34:56 น.
```

ตอนนี้, ลองเชื่อมคำสั่ง เรามาเรียงลำดับกระบวนการตามการใช้งานหน่วยความจำ:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

นี่จะแสดงกระบวนการ 5 อันดับแรกตามขนาดของ working set (การใช้หน่วยความจำ).

## ลึกซึ้ง
REPL ของ PowerShell มีรากฐานมาจาก shell Unix และ shell ภาษาไดนามิกอื่นๆ เช่น Python มันเป็นสิ่งแวดล้อมการปฏิบัติการคำสั่งแบบโต้ตอบเพื่อผู้ใช้เดียว ไม่เหมือนกับภาษาที่คอมไพล์ที่คุณเขียนแอปพลิเคชั่นทั้งหมดแล้วจึงคอมไพล์, สภาพแวดล้อม REPL ช่วยให้คุณเขียนและรันโค้ดทีละบรรทัด PowerShell ยังรองรับการทำงานสคริปต์สำหรับงานขนาดใหญ่

ทางเลือกอื่นสำหรับ Windows รวมถึง Command Prompt หรือ REPL ของภาษาเฉพาะอื่นๆ เช่น IPython ในโลกของ Unix/Linux, shells เช่น bash หรือ zsh ให้บริการฟังก์ชันใกล้เคียงกัน

การนำไปใช้งานของ PowerShell ใช้แอปพลิเคชั่นโฮสต์ในการรัน shell PowerShell.exe ใน Windows เป็นที่นิยมที่สุด แต่ทางเลือกอื่นๆ เช่น Integrated Scripting Environment (ISE) หรือ terminal ที่รวมใน Visual Studio Code ก็สามารถให้บริการเป็นโฮสต์ได้

## ดูเพิ่มเติม
- [เกี่ยวกับ PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
