---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:17.231778-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E32\u0E23\u0E2D\
  \u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E43\u0E19 Ruby \u0E19\u0E31\u0E49\u0E19\
  \u0E07\u0E48\u0E32\u0E22\u0E14\u0E32\u0E22 \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E43\u0E0A\u0E49 `File` class \u0E0B\u0E36\u0E48\u0E07\u0E43\u0E2B\
  \u0E49\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E15\u0E48\u0E32\u0E07\u0E46\u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C \u0E19\u0E35\
  \u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\
  \u0E32\u0E22\u0E46\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\
  \u0E1F\u0E25\u0E4C\u0E17\u0E31\u0E49\u0E07\u0E2B\u0E21\u0E14."
lastmod: '2024-03-17T21:57:56.745988-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E43\
  \u0E19 Ruby \u0E19\u0E31\u0E49\u0E19\u0E07\u0E48\u0E32\u0E22\u0E14\u0E32\u0E22 \u0E04\
  \u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49 `File` class\
  \ \u0E0B\u0E36\u0E48\u0E07\u0E43\u0E2B\u0E49\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\
  \u0E23\u0E15\u0E48\u0E32\u0E07\u0E46\u0E43\u0E19\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\
  \u0E19\u0E44\u0E1F\u0E25\u0E4C \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E46\u0E02\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E17\u0E31\u0E49\u0E07\
  \u0E2B\u0E21\u0E14."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 22
---

## วิธีการ:
การอ่านไฟล์ใน Ruby นั้นง่ายดาย คุณสามารถใช้ `File` class ซึ่งให้วิธีการต่างๆในการอ่านไฟล์ นี่คือตัวอย่างง่ายๆของการอ่านไฟล์ทั้งหมด:

```Ruby
File.open("example.txt", "r") do |file|
  puts file.read
end
```

หาก `example.txt` มีข้อความว่า "Hello, Ruby!" นี่คือสิ่งที่คุณจะได้รับ:

```
Hello, Ruby!
```

สำหรับการอ่านทีละบรรทัด:

```Ruby
File.foreach("example.txt") { |line| puts line }
```

ไฟล์ `example.txt` เดียวกัน ตอนนี้ผลลัพธ์จะเป็นทีละบรรทัด:

```
Hello, Ruby!
```

## ประเด็นลึกซึ้ง:
โดยประวัติศาสตร์ การอ่านไฟล์ได้เป็นคุณลักษณะหลักของภาษาการโปรแกรม ช่วยให้สามารถโต้ตอบกับระบบไฟล์

ใน Ruby คุณยังสามารถอ่านไฟล์ได้ด้วยเครื่องมือต่างๆ:

1. `IO` class: สำหรับการดำเนินการไฟล์ระดับต่ำ
2. `readlines` method: โหลดไฟล์ทั้งหมดเข้าสู่อาร์เรย์ โดยมีแต่ละบรรทัดเป็นองค์ประกอบ
3. `File.read`: วิธีที่รวดเร็วในการอ่านไฟล์ทั้งหมดเข้าสู่สตริง

มีการตีบวกที่ต้องพิจารณา: `File.read` เหมาะสำหรับไฟล์ขนาดเล็ก แต่อาจใช้หน่วยความจำมากเกินไปสำหรับไฟล์ขนาดใหญ่ นั่นคือเมื่อการอ่านทีละบรรทัดหรือเป็นก้อนมีค่ามาก

## ดูเพิ่มเติม:
- เอกสาร Ruby สำหรับ `File` class: [ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html)
- การอภิปรายใน Stack Overflow เกี่ยวกับการอ่านไฟล์ใน Ruby: [stackoverflow.com/questions/tagged/ruby+file-io](https://stackoverflow.com/questions/tagged/ruby+file-io)
