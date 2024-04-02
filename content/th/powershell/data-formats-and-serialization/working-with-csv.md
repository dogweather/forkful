---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:26.545188-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV (Comma-Separated Values) \u0E40\u0E1B\u0E47\u0E19\u0E07\u0E32\
  \u0E19\u0E17\u0E35\u0E48\u0E1E\u0E1A\u0E1A\u0E48\u0E2D\u0E22\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E41\u0E25\u0E30\
  \u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E15\u0E32\
  \u0E23\u0E32\u0E07\u0E17\u0E35\u0E48\u0E21\u0E35\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u2026"
lastmod: '2024-03-17T21:57:56.462763-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV (Comma-Separated Values) \u0E40\u0E1B\u0E47\u0E19\u0E07\u0E32\
  \u0E19\u0E17\u0E35\u0E48\u0E1E\u0E1A\u0E1A\u0E48\u0E2D\u0E22\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E41\u0E25\u0E30\
  \u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E15\u0E32\
  \u0E23\u0E32\u0E07\u0E17\u0E35\u0E48\u0E21\u0E35\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## อะไร & ทำไม?

การทำงานกับไฟล์ CSV (Comma-Separated Values) เป็นงานที่พบบ่อยสำหรับการจัดการและดำเนินการกับข้อมูลในรูปแบบตารางที่มีโครงสร้าง โปรแกรมเมอร์มักต้องดำเนินการนี้เพื่อนำเข้า ส่งออก หรือดัดแปลงข้อมูลอย่างมีประสิทธิภาพสำหรับการใช้งานหลายประเภท เช่น การวิเคราะห์ข้อมูล การรายงาน หรือแม้แต่การขับเคลื่อนเว็บแอปพลิเคชั่น

## วิธีการ:

### การอ่านไฟล์ CSV

เพื่ออ่านจากไฟล์ CSV ให้ใช้ cmdlet `Import-Csv` คำสั่งนี้อ่านไฟล์และแปลงเป็นวัตถุ PowerShell ที่กำหนดเองสำหรับแต่ละแถว

```powershell
# การนำเข้าไฟล์ CSV
$data = Import-Csv -Path "C:\Data\users.csv"
# แสดงเนื้อหา
$data
```

**ตัวอย่างผลลัพธ์:**

```
Name    Age    City
----    ---    ----
John    23     New York
Doe     29     Los Angeles
```

### การเขียนลงไฟล์ CSV

ในทางตรงกันข้าม, เพื่อเขียนข้อมูลลงในไฟล์ CSV, จะใช้ cmdlet `Export-Csv` คำสั่งนี้รับวัตถุเข้าและแปลงเป็นรูปแบบ CSV

```powershell
# การสร้างวัตถุเพื่อส่งออก
$users = @(
    [PSCustomObject]@{Name='John'; Age='23'; City='New York'},
    [PSCustomObject]@{Name='Doe'; Age='29'; City='Los Angeles'}
)

# การส่งออกเป็นไฟล์ CSV
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

หลังจากทำการปฏิบัติ, ไฟล์ที่ชื่อว่า `new_users.csv` ถูกสร้างขึ้นด้วยข้อมูลที่ให้มา

### การกรองและจัดการเนื้อหา CSV

เพื่อกรองหรือจัดการข้อมูลจากไฟล์ CSV, ใช้ความสามารถในการจัดการวัตถุของ PowerShell เช่น, เพื่อเลือกเฉพาะผู้ใช้ที่มีอายุมากกว่าค่าที่กำหนดและมาจากเมืองที่ระบุ:

```powershell
# การนำเข้าและกรองข้อมูล
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'Los Angeles'
}

# แสดงข้อมูลที่ถูกกรอง
$filteredData
```

**ตัวอย่างผลลัพธ์:**

```
Name    Age    City
----    ---    ----
Doe     29     Los Angeles
```

### การใช้ไลบรารี่ของบุคคลที่สาม

ในขณะที่ cmdlets ของ PowerShell โดยเนื้อแท้มักเพียงพอสำหรับงานทั่วไป การดำเนินการที่ซับซ้อนมากขึ้นอาจได้ประโยชน์จากไลบรารี่หรือเครื่องมือของบุคคลที่สาม อย่างไรก็ดี สำหรับการจัดการ CSV มาตรฐาน เช่น การอ่าน, การเขียน, การกรอง หรือการเรียงลำดับ cmdlets ในตัวของ PowerShell เช่น `Import-Csv` และ `Export-Csv` มักเสนอฟังก์ชันการทำงานที่แข็งแกร่งโดยไม่จำเป็นต้องใช้ไลบรารี่เพิ่มเติม
