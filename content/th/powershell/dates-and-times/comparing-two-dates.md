---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:01.202853-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E2D\u0E07\u0E27\u0E31\u0E19\
  \u0E43\u0E19 PowerShell \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\
  \u0E2B\u0E32\u0E27\u0E48\u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2B\u0E19\u0E36\
  \u0E48\u0E07\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u0E01\u0E48\u0E2D\u0E19\
  \ \u0E43\u0E19\u0E40\u0E27\u0E25\u0E32\u0E40\u0E14\u0E35\u0E22\u0E27\u0E01\u0E31\
  \u0E19 \u0E2B\u0E23\u0E37\u0E2D\u0E2B\u0E25\u0E31\u0E07\u0E08\u0E32\u0E01\u0E2D\u0E35\
  \u0E01\u0E27\u0E31\u0E19\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48\u2026"
lastmod: '2024-03-17T21:57:56.453486-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E2D\u0E07\u0E27\u0E31\u0E19\
  \u0E43\u0E19 PowerShell \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\
  \u0E2B\u0E32\u0E27\u0E48\u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2B\u0E19\u0E36\
  \u0E48\u0E07\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u0E01\u0E48\u0E2D\u0E19\
  \ \u0E43\u0E19\u0E40\u0E27\u0E25\u0E32\u0E40\u0E14\u0E35\u0E22\u0E27\u0E01\u0E31\
  \u0E19 \u0E2B\u0E23\u0E37\u0E2D\u0E2B\u0E25\u0E31\u0E07\u0E08\u0E32\u0E01\u0E2D\u0E35\
  \u0E01\u0E27\u0E31\u0E19\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48 \u0E19\u0E31\u0E01\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E21\u0E31\u0E01\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\
  \u0E1A\u0E40\u0E2B\u0E15\u0E38\u0E01\u0E32\u0E23\u0E13\u0E4C \u0E08\u0E31\u0E14\u0E40\
  \u0E23\u0E35\u0E22\u0E07\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01 \u0E27\u0E32\u0E07\u0E41\
  \u0E1C\u0E19\u0E07\u0E32\u0E19 \u0E2B\u0E23\u0E37\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\
  \u0E2D\u0E1A\u0E2D\u0E32\u0E22\u0E38\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25."
title: "\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\u0E22\u0E1A\u0E2A\u0E2D\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48"
weight: 27
---

## วิธีทำ:
```PowerShell
# มาเริ่มจากการเอาวันที่ของวันนี้มาก่อน
$today = Get-Date

# และนี่คือวันที่เฉพาะเจาะจงหนึ่งวัน
$someOtherDate = Get-Date "2023-03-17"

# พวกมันเท่ากันหรือไม่?
$today -eq $someOtherDate

# วันนี้มีค่ามากกว่า (หลังจาก) วันที่อื่นหรือไม่?
$today -gt $someOtherDate

# แล้วถ้าต้องการตรวจสอบว่ามันเกิดขึ้นก่อนหรือไม่ล่ะ?
$today -lt $someOtherDate

# มาดูผลลัพธ์กันเถอะ

False
True
False
```

## ลงลึก
ย้อนกลับไปในยุคหินของการคอมพิวติ้ง—ไม่จริงด้วยซ้ำ แต่คุณรู้ล่ะ ยุคแรกๆ—วันที่เป็นเรื่องยุ่งยาก เราได้พัฒนาไกลอย่างมากด้วยมาตรฐาน และ PowerShell ได้ทำให้มันง่ายขึ้นอีก

นี่คือส่วนที่ควรตระหนักถึง:
1. **ประวัติ**: คอมพิวเตอร์เคยจัดการกับวันที่ในรูปแบบต่างๆ ทำให้เกิดความสับสนและบักแบบ Y2K ได้ PowerShell ใช้โครงสร้าง `DateTime` ของ .NET ช่วยหลีกเลี่ยงความโกลาหลนั้น

2. **ทางเลือก**: คุณยังสามารถใช้ `Compare-Object` หรือใช้วิธีการจากอ็อบเจกต์ `[datetime]` เช่น `.AddDays()` ในการทำการคำนวณก่อนการเปรียบเทียบ จำไว้ว่าให้ใช้ `Measure-Command` เพื่อทดสอบผลกระทบด้านประสิทธิภาพ

3. **รายละเอียดการดำเนินการ**: วันที่ใน PowerShell เป็นอ็อบเจกต์ที่มีคุณสมบัติและวิธีการของตัวเอง การเปรียบเทียบวันที่ทำได้ด้วยตัวดำเนินการ (`-eq`, `-lt`, `-gt`), และด้วยการโอเวอร์โหลดตัวดำเนินการ PowerShell รู้ว่าคุณกำลังจัดการกับวันที่ ไม่ใช่แค่สตริงหรือตัวเลข

ในระดับแอสเซมบลี การเปรียบเทียบวันที่แปลเป็นการเปรียบเทียบจำนวนเติ๊ก (ช่วง 100 นาโนวินาทีตั้งแต่วันที่ 1/1/0001) ดังนั้นคุณจึงเปรียบเทียบจำนวนเต็มขนาดใหญ่ ซึ่งมีประสิทธิภาพ

## ดูเพิ่มเติม
- [โครงสร้าง DateTime (เอกสารของ Microsoft)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [การทำงานกับวันที่และเวลาใน PowerShell (SS64.com)](https://ss64.com/ps/syntax-dateformats.html)
