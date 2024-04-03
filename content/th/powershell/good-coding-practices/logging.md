---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:15.651424-06:00
description: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01 (Logging) \u0E19\
  \u0E31\u0E49\u0E19\u0E1E\u0E39\u0E14\u0E07\u0E48\u0E32\u0E22\u0E46 \u0E01\u0E47\u0E04\
  \u0E37\u0E2D\u0E01\u0E32\u0E23\u0E17\u0E34\u0E49\u0E07\u0E40\u0E2A\u0E49\u0E19\u0E17\
  \u0E32\u0E07\u0E40\u0E25\u0E47\u0E01\u0E46 \u0E1C\u0E48\u0E32\u0E19\u0E42\u0E04\u0E49\
  \u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 -\u2026"
lastmod: '2024-03-17T21:57:56.447950-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01 (Logging) \u0E19\u0E31\
  \u0E49\u0E19\u0E1E\u0E39\u0E14\u0E07\u0E48\u0E32\u0E22\u0E46 \u0E01\u0E47\u0E04\u0E37\
  \u0E2D\u0E01\u0E32\u0E23\u0E17\u0E34\u0E49\u0E07\u0E40\u0E2A\u0E49\u0E19\u0E17\u0E32\
  \u0E07\u0E40\u0E25\u0E47\u0E01\u0E46 \u0E1C\u0E48\u0E32\u0E19\u0E42\u0E04\u0E49\u0E14\
  \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 - \u0E21\u0E31\u0E19\u0E04\u0E37\u0E2D\u0E27\
  \u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E15\u0E34\u0E14\u0E15\u0E32\
  \u0E21\u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\u0E48\u0E01\u0E33\u0E25\u0E31\u0E07\u0E40\
  \u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u0E40\u0E21\u0E37\u0E48\u0E2D\u0E2A\u0E04\
  \u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E01\u0E33\u0E25\
  \u0E31\u0E07\u0E17\u0E33\u0E07\u0E32\u0E19\u0E43\u0E19\u0E2A\u0E16\u0E32\u0E19\u0E01\
  \u0E32\u0E23\u0E13\u0E4C\u0E08\u0E23\u0E34\u0E07\u0E46 \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E01\u0E49\u0E08\u0E38\u0E14\u0E1A\u0E01\u0E1E\u0E23\
  \u0E48\u0E2D\u0E07, \u0E15\u0E34\u0E14\u0E15\u0E32\u0E21\u0E1E\u0E24\u0E15\u0E34\
  \u0E01\u0E23\u0E23\u0E21\u0E41\u0E2D\u0E1B, \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\
  \u0E1A\u0E1B\u0E23\u0E30\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E20\u0E32\u0E1E, \u0E41\
  \u0E25\u0E30\u0E08\u0E31\u0E1A\u0E15\u0E32\u0E14\u0E39\u0E01\u0E32\u0E23\u0E01\u0E23\
  \u0E30\u0E17\u0E33\u0E17\u0E35\u0E48\u0E44\u0E21\u0E48\u0E40\u0E2B\u0E21\u0E32\u0E30\
  \u0E2A\u0E21\u0E43\u0E14\u0E46."
title: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E25\u0E47\u0E2D\u0E01"
weight: 17
---

## วิธีการ:
นี่คือข้อมูลพื้นฐานในการเพิ่มการบันทึกแบบพื้นฐานลงในสคริปต์ของคุณ:

```PowerShell
# การสร้างข้อความบันทึกง่ายๆ
Write-Host "Info: เริ่มกระบวนการสคริปต์."

# การเขียนลงไฟล์
"Info: นี่คือข้อความที่ถูกบันทึก." | Out-File -Append myLog.log

# การใช้ cmdlet ตัวในเพื่อบันทึกข้อมูลแบบละเอียด
Start-Transcript -Path "./detailedLog.log"
Write-Output "Warning: บางอย่างไม่ถูกต้อง."
# ... สคริปต์ของคุณทำงาน
Stop-Transcript

# ผลลัพธ์ของ detailedLog.log
******************************
Windows PowerShell transcript start
Start time: 20230324112347
Username  : PShellGuru@example.com
RunAs User: PShellGuru@example.com
Configuration Name: 
Machine  : PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
Host Application: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
Process ID: 2024
PS Version: 7.1.2
```

ตอนนี้, ในบันทึกของคุณ, มีการบันทึกแบบเล่นๆ ของสิ่งที่โค้ดของคุณได้ทำไป.

## การศึกษาลึก:
ตามประวัติศาสตร์, การบันทึกเก่าแก่เท่ากับการเขียนโปรแกรมเอง มันเหมือนกับบันทึกการเดินเรือแต่สำหรับซอฟต์แวร์ ในอดีต, อาจจะเป็นการพิมพ์ออกมาหรือเครื่องพิมพ์โทรเลข; ตอนนี้มันกลายเป็นเรื่องเกี่ยวกับไฟล์และระบบการจัดการบันทึกข้อมูลที่สวยงาม

เมื่อคุณกำลังอยู่ในแนวรบแห่ง PowerShell, `Write-Host` รวดเร็วและสกปรก, แต่มันเพียงแค่ส่งข้อความออกไปยังคอนโซล, ไม่ดีสำหรับการเก็บบันทึก `Out-File` ให้คุณวิธีง่ายๆ ในการส่งข้อความเข้าไฟล์, แต่สำหรับน้ำยาที่แท้จริง, คุณต้องการ `Start-Transcript` และ `Stop-Transcript` ซึ่งบันทึกทุกอย่าง—การป้อนข้อมูล, ผลลัพธ์, จริงๆ ทั้งหมด

ทางเลือก? แน่นอน, หากคุณกำลังทำงานระดับองค์กร, คุณอาจดูที่ Windows Event Log หรือใช้ซอฟต์แวร์เช่น Logstash, แต่สำหรับสคริปต์ประจำวันของคุณ, ให้ยึดติดกับเครื่องมือของ PowerShell สำหรับการนำไปใช้, จำไว้ว่าต้องบันทึกอย่างฉลาด - น้อยเกินไปและมันจะไม่มีประโยชน์, มากเกินไปและมันจะเป็นเพียงเสียงรบกวน

## ดูเพิ่มเติม:
ดูสิ่งเหล่านี้เพื่อเข้าใจเรื่องการบันทึกใน PowerShell ทั้งหมด:
