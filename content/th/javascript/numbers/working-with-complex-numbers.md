---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:47.253365-06:00
description: "\u0E08\u0E33\u0E19\u0E27\u0E19\u0E40\u0E0A\u0E34\u0E07\u0E0B\u0E49\u0E2D\
  \u0E19\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E17\u0E35\u0E48\
  \u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22\u0E2A\u0E48\u0E27\u0E19\
  \u0E08\u0E23\u0E34\u0E07\u0E41\u0E25\u0E30\u0E2A\u0E48\u0E27\u0E19\u0E08\u0E34\u0E19\
  \u0E15\u0E20\u0E32\u0E1E (\u0E40\u0E0A\u0E48\u0E19 3 + 4i) \u0E1E\u0E27\u0E01\u0E21\
  \u0E31\u0E19\u0E1B\u0E23\u0E32\u0E01\u0E0F\u0E43\u0E19\u0E1B\u0E31\u0E0D\u0E2B\u0E32\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E15\u0E48\u0E32\u0E07\u0E46 \u0E42\u0E14\u0E22\u0E40\u0E09\u0E1E\u0E32\u0E30\
  \u0E43\u0E19\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2A\
  \u0E31\u0E0D\u0E0D\u0E32\u0E13,\u2026"
lastmod: '2024-03-17T21:57:56.595855-06:00'
model: gpt-4-0125-preview
summary: "\u0E08\u0E33\u0E19\u0E27\u0E19\u0E40\u0E0A\u0E34\u0E07\u0E0B\u0E49\u0E2D\
  \u0E19\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E17\u0E35\u0E48\
  \u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22\u0E2A\u0E48\u0E27\u0E19\
  \u0E08\u0E23\u0E34\u0E07\u0E41\u0E25\u0E30\u0E2A\u0E48\u0E27\u0E19\u0E08\u0E34\u0E19\
  \u0E15\u0E20\u0E32\u0E1E (\u0E40\u0E0A\u0E48\u0E19 3 + 4i) \u0E1E\u0E27\u0E01\u0E21\
  \u0E31\u0E19\u0E1B\u0E23\u0E32\u0E01\u0E0F\u0E43\u0E19\u0E1B\u0E31\u0E0D\u0E2B\u0E32\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E15\u0E48\u0E32\u0E07\u0E46 \u0E42\u0E14\u0E22\u0E40\u0E09\u0E1E\u0E32\u0E30\
  \u0E43\u0E19\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2A\
  \u0E31\u0E0D\u0E0D\u0E32\u0E13,\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
จำนวนเชิงซ้อนเป็นตัวเลขที่ประกอบด้วยส่วนจริงและส่วนจินตภาพ (เช่น 3 + 4i) พวกมันปรากฏในปัญหาการเขียนโปรแกรมต่างๆ โดยเฉพาะในการประมวลผลสัญญาณ, การคำนวณควอนตัม และการแก้สมการพหุนาม โปรแกรมเมอร์ใช้พวกมันเพื่อจัดการกับงานเหล่านี้ได้อย่างมีประสิทธิภาพ

## วิธีทำ:
JavaScript ไม่มีการสนับสนุนจำนวนเชิงซ้อนโดยตรง แต่คุณสามารถใช้วัตถุและคณิตศาสตร์เพื่อจัดการกับมันได้ นี่คือวิธีการที่รวดเร็ว

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...เพิ่มเมธอดเพิ่มเติม (ลบ, คูณ, หาร) ตามที่ต้องการ

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const result = a.add(b);

console.log(`Result: ${result}`); // ผลลัพธ์: 4 + 6i
```

## การศึกษาลึก
จำนวนเชิงซ้อนมีอยู่มาตั้งแต่ศตวรรษที่ 16 ขอบคุณนักคณิตศาสตร์ชาวอิตาลี Gerolamo Cardano พวกมันกลายเป็นสิ่งสำคัญในหลายสาขา เช่น วิศวกรรมและฟิสิกส์ ในการเขียนโปรแกรมสมัยใหม่ พวกเขาเป็นกุญแจสำคัญสำหรับการจำลองและอัลกอริทึมที่ต้องการความหลากหลายมิติ

ตอนนี้ JavaScript ไม่ได้รองรับจำนวนเชิงซ้อนโดยธรรมชาติ แต่นอกเหนือจากตัวเลือก DIY คุณอาจใช้ไลบรารีคณิตศาสตร์ เช่น math.js หรือ numeric.js พวกเขาให้พลังการประมวลผลเพิ่มเติมสำหรับการจัดการจำนวนเชิงซ้อน รวมถึงการคำนวณขนาด, และการค้นหาอาร์กิวเมนต์

ภายใต้ฝาครอบ ในเมื่อคุณทำงานกับจำนวนเชิงซ้อน เหมือนกับว่าคุณกำลังจัดการกับสองตัวเลขที่ถูกผูกมัดเข้าด้วยกัน การบวกและการลบเป็นการเล่นที่ตรงไปตรงมา—จับคู่ตัวเลขจริงกับจริง, จินตภาพกับจินตภาพ การคูณและการหารได้รับความสนใจมากขึ้นพร้อมกับการผสมข้ามเทอมและต้องการความใส่ใจมากขึ้น

## ดูเพิ่มเติม
- MDN Web Docs บน JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, ไลบรารีคณิตศาสตร์รวมจำนวนเชิงซ้อน: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, ไลบรารีอื่น: http://numericjs.com/documentation.html
- การศึกษาเพิ่มเติมเกี่ยวกับจำนวนเชิงซ้อน (มุ่งเน้นคณิตศาสตร์): https://mathworld.wolfram.com/ComplexNumber.html
