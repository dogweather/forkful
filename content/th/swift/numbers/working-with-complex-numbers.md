---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:29.727775-06:00
description: "\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\
  \u0E19\u0E21\u0E35\u0E2A\u0E48\u0E27\u0E19\u0E08\u0E23\u0E34\u0E07\u0E41\u0E25\u0E30\
  \u0E2A\u0E48\u0E27\u0E19\u0E08\u0E34\u0E19\u0E15\u0E20\u0E32\u0E1E (\u0E40\u0E0A\
  \u0E48\u0E19 3 + 4i) \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E43\u0E19 Swift \u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E40\u0E0A\u0E48\u0E19\u0E01\u0E32\u0E23\u0E1B\
  \u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2A\u0E31\u0E0D\u0E0D\u0E32\u0E13,\
  \ \u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E1B\u0E31\u0E0D\u0E2B\u0E32\u0E04\u0E13\
  \u0E34\u0E15\u0E28\u0E32\u0E2A\u0E15\u0E23\u0E4C\u0E1A\u0E32\u0E07\u0E2D\u0E22\u0E48\
  \u0E32\u0E07,\u2026"
lastmod: '2024-03-17T21:57:56.555938-06:00'
model: gpt-4-0125-preview
summary: "\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\
  \u0E19\u0E21\u0E35\u0E2A\u0E48\u0E27\u0E19\u0E08\u0E23\u0E34\u0E07\u0E41\u0E25\u0E30\
  \u0E2A\u0E48\u0E27\u0E19\u0E08\u0E34\u0E19\u0E15\u0E20\u0E32\u0E1E (\u0E40\u0E0A\
  \u0E48\u0E19 3 + 4i) \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E43\u0E19 Swift \u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E40\u0E0A\u0E48\u0E19\u0E01\u0E32\u0E23\u0E1B\
  \u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2A\u0E31\u0E0D\u0E0D\u0E32\u0E13,\
  \ \u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E1B\u0E31\u0E0D\u0E2B\u0E32\u0E04\u0E13\
  \u0E34\u0E15\u0E28\u0E32\u0E2A\u0E15\u0E23\u0E4C\u0E1A\u0E32\u0E07\u0E2D\u0E22\u0E48\
  \u0E32\u0E07,\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19"
---

{{< edit_this_page >}}

## คืออะไร & ทำไม?
ตัวเลขซับซ้อนมีส่วนจริงและส่วนจินตภาพ (เช่น 3 + 4i) โปรแกรมเมอร์ใช้งานใน Swift สำหรับงานเช่นการประมวลผลสัญญาณ, การแก้ปัญหาคณิตศาสตร์บางอย่าง, และการจำลองฟิสิกส์

## วิธีการ:
Swift ไม่มีการสนับสนุนตัวเลขซับซ้อนอย่างในตัว แต่เราสามารถสร้างของเราเองได้:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // วิธีการเพิ่มเติมเช่น การลบ, การคูณ ฯลฯ
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("Result: \(result.real) + \(result.imaginary)i")
// ตัวอย่างผลลัพธ์: Result: 3.0 + 7.0i
```

## ดำดิ่งลึก
ตัวเลขซับซ้อนปรากฏขึ้นในศตวรรษที่ 16 ในสมการพีชคณิต พวกเขามีความสำคัญในกลศาสตร์ควอนตัม, ทฤษฎีการควบคุม, และสาขาอื่นๆ อีกมากมาย Apple's Swift ไม่มีไลบรารีมาตรฐานสำหรับตัวเลขซับซ้อน ไม่เหมือนกับภาษาเช่น Python หรือ C++ ทางเลือกในการสร้างของคุณเอง รวมถึงการใช้งานแพ็กเกจ Numerics ซึ่งรวมถึงการสนับสนุนตัวเลขซับซ้อน หรือห่อหุ้มไลบรารีซับซ้อนของ C++ ด้วยความสามารถในการทำงานร่วมกันของ Swift

## ดูเพิ่มเติม
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
