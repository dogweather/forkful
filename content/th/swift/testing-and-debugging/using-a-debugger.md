---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:48.595219-06:00
description: "\u0E43\u0E19\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E14\u0E1A\u0E31\
  \u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\u0E43\u0E19 Xcode (IDE \u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A Swift), \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E15\
  \u0E31\u0E49\u0E07\u0E08\u0E38\u0E14\u0E2B\u0E22\u0E38\u0E14, \u0E15\u0E23\u0E27\
  \u0E08\u0E2A\u0E2D\u0E1A\u0E04\u0E48\u0E32\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23,\
  \ \u0E41\u0E25\u0E30\u0E08\u0E31\u0E1A\u0E15\u0E32\u0E14\u0E39\u0E19\u0E34\u0E1E\
  \u0E08\u0E19\u0E4C\u0E44\u0E14\u0E49 \u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E40\u0E0A\u0E48\u0E19: ```Swift func findFactorial(of\u2026"
lastmod: '2024-03-17T21:57:56.566122-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E14\u0E1A\u0E31\
  \u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\u0E43\u0E19 Xcode (IDE \u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A Swift), \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E15\
  \u0E31\u0E49\u0E07\u0E08\u0E38\u0E14\u0E2B\u0E22\u0E38\u0E14, \u0E15\u0E23\u0E27\
  \u0E08\u0E2A\u0E2D\u0E1A\u0E04\u0E48\u0E32\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23,\
  \ \u0E41\u0E25\u0E30\u0E08\u0E31\u0E1A\u0E15\u0E32\u0E14\u0E39\u0E19\u0E34\u0E1E\
  \u0E08\u0E19\u0E4C\u0E44\u0E14\u0E49 \u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E40\u0E0A\u0E48\u0E19:\n\n```Swift\nfunc findFactorial(of number: Int) -> Int\
  \ {\n    if number == 0 {\n        return 1\n    }\n    return number * findFactorial(of:\
  \ number - 1)\n}\n\nlet result = findFactorial(of: 5)\nprint(result)\n```\n\n\u0E15\
  \u0E31\u0E49\u0E07\u0E08\u0E38\u0E14\u0E2B\u0E22\u0E38\u0E14\u0E42\u0E14\u0E22\u0E01\
  \u0E32\u0E23\u0E04\u0E25\u0E34\u0E01\u0E0B\u0E49\u0E32\u0E22\u0E44\u0E1B\u0E17\u0E35\
  \u0E48\u0E2B\u0E21\u0E32\u0E22\u0E40\u0E25\u0E02\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\
  \u0E43\u0E19 Xcode, \u0E41\u0E25\u0E30\u0E40\u0E23\u0E35\u0E22\u0E01\u0E43\u0E0A\
  \u0E49\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21 \u0E40\u0E21\u0E37\u0E48\u0E2D\u0E21\
  \u0E31\u0E19\u0E16\u0E36\u0E07\u0E08\u0E38\u0E14\u0E2B\u0E22\u0E38\u0E14, Xcode\
  \ \u0E08\u0E30\u0E2B\u0E22\u0E38\u0E14\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\
  \u0E19 \u0E15\u0E2D\u0E19\u0E19\u0E35\u0E49\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16:\n\n1."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
weight: 35
---

## วิธีการ:
ในการใช้เดบักเกอร์ใน Xcode (IDE สำหรับ Swift), คุณสามารถตั้งจุดหยุด, ตรวจสอบค่าตัวแปร, และจับตาดูนิพจน์ได้ ตัวอย่างเช่น:

```Swift
func findFactorial(of number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(of: number - 1)
}

let result = findFactorial(of: 5)
print(result)
```

ตั้งจุดหยุดโดยการคลิกซ้ายไปที่หมายเลขบรรทัดใน Xcode, และเรียกใช้โปรแกรม เมื่อมันถึงจุดหยุด, Xcode จะหยุดการทำงาน ตอนนี้คุณสามารถ:

1. ตรวจสอบค่าของตัวแปร
2. ข้ามไปข้างหน้า (เรียกใช้บรรทัดถัดไป) หรือข้ามเข้าไปใน (เข้าไปในฟังก์ชัน) โดยใช้การควบคุมเดบักเกอร์
3. เพิ่มนิพจน์ไปยัง 'รายการจับตาดู' เพื่อตรวจสอบการเปลี่ยนแปลงของตัวแปรหรือค่าคงที่เฉพาะ

นี่คือสิ่งที่คุณอาจเห็นในพื้นที่ debug:

```
(lldb) po number
5
(lldb) po result
120
```

## ดำดิ่งลึก:
เดบักเกอร์ได้เป็นส่วนหนึ่งของภูมิทัศน์การเขียนโปรแกรมตั้งแต่ยุค 1940s, พัฒนาจากระบบจุดหยุดง่ายๆ ไปจนถึงประสบการณ์ที่ซับซ้อนและใช้งานผ่าน UI ตัวเลือกอื่นๆ นอกเหนือจากเดบักเกอร์ที่ฝังอยู่ใน Xcode รวมถึงเครื่องมือของบุคคลที่สามเช่น LLDB (Low Level Debugger) ซึ่ง Xcode ใช้ภายใต้หลังคา บางคนแม้แต่ตัดสินใจใช้คำสั่ง `print()` (ที่รู้จักกันดีในชื่อ "การดีบักแบบมนุษย์ถ้ำ"), แต่นี่น้อยกว่าเป็นประสิทธิภาพสำหรับโครงการขนาดใหญ่หรือข้อบกพร่องที่ซับซ้อน เมื่อคุณใช้เดบักเกอร์, คุณกำลังจัดการกับการควบคุมการดำเนินการ, การตรวจสอบรันไทม์, และการจัดการข้อมูล ความเข้าใจลึกซึ้งในหลักการเหล่านี้มีความสำคัญในการดีบักอย่างมีประสิทธิภาพ

## ดูเพิ่มเติม:
- [แนะนำการดีบักของ Xcode โดย Apple](https://developer.apple.com/documentation/xcode/debugging/)
- [คู่มือเริ่มต้นใช้งาน LLDB อย่างรวดเร็ว](https://lldb.llvm.org/use/tutorial.html)
- [บทช่วยสอนการดีบัก Swift โดย Ray Wenderlich](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
