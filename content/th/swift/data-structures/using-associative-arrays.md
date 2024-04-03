---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:20.889406-06:00
description: "Associative arrays \u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E35\u0E48\u0E23\u0E39\
  \u0E49\u0E08\u0E31\u0E01\u0E43\u0E19 Swift \u0E27\u0E48\u0E32 dictionaries \u0E0A\
  \u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E40\u0E01\u0E47\u0E1A\u0E41\
  \u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E04\u0E39\u0E48\u0E04\u0E35\u0E22\
  \u0E4C-\u0E04\u0E48\u0E32 (key-value pairs)\u2026"
lastmod: '2024-03-17T21:57:56.555063-06:00'
model: gpt-4-0125-preview
summary: "Associative arrays \u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E35\u0E48\u0E23\u0E39\
  \u0E49\u0E08\u0E31\u0E01\u0E43\u0E19 Swift \u0E27\u0E48\u0E32 dictionaries \u0E0A\
  \u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E40\u0E01\u0E47\u0E1A\u0E41\
  \u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E04\u0E39\u0E48\u0E04\u0E35\u0E22\
  \u0E4C-\u0E04\u0E48\u0E32 (key-value pairs) \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E21\u0E35\u0E1B\u0E23\
  \u0E30\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E20\u0E32\u0E1E \u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E41\u0E25\u0E30\u0E08\
  \u0E31\u0E14\u0E01\u0E32\u0E23\u0E04\u0E48\u0E32\u0E15\u0E32\u0E21\u0E04\u0E35\u0E22\
  \u0E4C\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E2D\u0E01\u0E25\u0E31\u0E01\
  \u0E29\u0E13\u0E4C\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E41\u0E2D\u0E40\u0E23\u0E22\u0E4C\u0E2A\
  \u0E21\u0E32\u0E0A\u0E34\u0E01"
weight: 15
---

## อะไร & ทำไม?

Associative arrays หรือที่รู้จักใน Swift ว่า dictionaries ช่วยให้คุณเก็บและจัดการข้อมูลในรูปแบบคู่คีย์-ค่า (key-value pairs) โปรแกรมเมอร์ใช้มันเพื่อจัดระเบียบข้อมูลอย่างมีประสิทธิภาพ ทำให้การเข้าถึงและจัดการค่าตามคีย์ที่เป็นเอกลักษณ์ง่ายขึ้น

## วิธีการ:

Swift ทำให้การทำงานกับ associative arrays เป็นเรื่องง่าย นี่คือวิธีที่คุณสามารถประกาศ เพิ่ม ลบ และเข้าถึงรายการใน Swift dictionary:

```Swift
// การประกาศ dictionary
var fruitColors: [String: String] = ["Apple": "Red", "Banana": "Yellow"]

// เพิ่มรายการใหม่
fruitColors["Grape"] = "Purple"

// เข้าถึงค่าโดยใช้คีย์
if let appleColor = fruitColors["Apple"] {
    print("Apple is \(appleColor).")  // ผลลัพธ์: Apple is Red.
} else {
    print("ไม่พบสี.")
}

// การลบรายการ
fruitColors["Banana"] = nil  // สิ่งนี้จะลบ "Banana" ออกจาก dictionary

// การวนซ้ำรายการ
for (fruit, color) in fruitColors {
    print("\(fruit) is \(color).")
    // ผลลัพธ์:
    // Apple is Red.
    // Grape is Purple.
}
```

Dictionaries มีความยืดหยุ่นอย่างมาก ช่วยให้คุณสามารถจัดการและเข้าถึงข้อมูลได้อย่างไดนามิก ลักษณะที่ไม่มีการเรียงลำดับไม่ส่งผลต่อความเร็วในการเรียกข้อมูล ซึ่งเป็นข้อได้เปรียบสำคัญเมื่อต้องจัดการกับชุดข้อมูลขนาดใหญ่

## การเจาะลึก

การที่ Swift ใช้การดำเนินการด้วย dictionaries ในฐานะ associative array มีรากฐานมาจากความสามารถอันทรงพลังในการจับคู่คีย์ที่ไม่ซ้ำกันกับค่า ในประวัติศาสตร์ ภาษาโปรแกรมได้ใช้แนวคิดนี้ภายใต้ชื่อต่างๆ เช่น hash tables หรือ maps ซึ่งบ่งบอกถึงการทำงานของพวกเขาในการสร้าง "แผนที่" ระหว่างคีย์กับค่า

ใน Swift dictionaries ถูกปรับให้มีประสิทธิภาพสูง โดยใช้คีย์ที่สามารถ hash ได้สำหรับการเรียกข้อมูลอย่างมีประสิทธิภาพ นี่หมายความว่า ประเภท `Key` ใน dictionary แบบ `[Key: Value]` ต้องสอดคล้องกับโปรโตคอล `Hashable`ซึ่งเป็นกรณีสำหรับประเภทมาตรฐานของ Swift ส่วนใหญ่เช่น `Int`, `String`, และ `Double`

หนึ่งสิ่งที่ต้องพิจารณาคือ แม้ dictionaries จะเหมาะสำหรับการเชื่อมโยงคู่ของข้อมูล แต่ก็ขาดการเรียงลำดับ หากคุณต้องการรักษาลำดับขององค์ประกอบ คุณอาจสำรวจทางเลือกอื่น เช่น `Array` สำหรับลำดับขององค์ประกอบที่เรียงลำดับ หรือโครงสร้างข้อมูลที่กำหนดเองที่ผสมผสานคุณสมบัติของทั้ง arrays และ dictionaries

นอกจากนี้ ยังน่าสังเกตว่า Swift ยังคงพัฒนาอย่างต่อเนื่อง และการจัดการและการปรับให้ดีขึ้นของ dictionaries ก็เช่นกัน ดังนั้นการติดตามเอกสาร Swift ล่าสุดจึงเป็นสิ่งสำคัญเพื่อให้คุณสามารถใช้ประโยชน์สูงสุดจาก dictionaries ได้ โดยให้คุณใช้ปฏิบัติการที่มีประสิทธิภาพและทันสมัยที่สุด
