---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:52.624260-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Google Apps Script \u0E44\
  \u0E21\u0E48\u0E21\u0E35\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\
  \u0E19\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19\u0E2D\u0E22\u0E48\
  \u0E32\u0E07\u0E43\u0E19\u0E15\u0E31\u0E27, \u0E08\u0E33\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E49\u0E2D\u0E07\u0E21\u0E35\u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\
  \u0E19\u0E01\u0E32\u0E23\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E40\u0E2D\u0E07\u2026"
lastmod: '2024-03-17T21:57:55.709563-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u0E44\u0E21\u0E48\u0E21\u0E35\u0E01\u0E32\u0E23\u0E2A\
  \u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\
  \u0E49\u0E2D\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E43\u0E19\u0E15\u0E31\u0E27,\
  \ \u0E08\u0E33\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E49\u0E2D\u0E07\u0E21\u0E35\u0E01\
  \u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E1F\u0E31\u0E07\
  \u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E40\
  \u0E2D\u0E07 \u0E14\u0E49\u0E32\u0E19\u0E25\u0E48\u0E32\u0E07\u0E19\u0E35\u0E49\u0E40\
  \u0E1B\u0E47\u0E19\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E1E\u0E37\
  \u0E49\u0E19\u0E10\u0E32\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E40\u0E25\u0E02\u0E0B\u0E31\
  \u0E1A\u0E0B\u0E49\u0E2D\u0E19 \u0E23\u0E27\u0E21\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\
  \u0E1A\u0E27\u0E01, \u0E01\u0E32\u0E23\u0E25\u0E1A, \u0E41\u0E25\u0E30\u0E01\u0E32\
  \u0E23\u0E04\u0E39\u0E13."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19"
weight: 14
---

## วิธีการ:
Google Apps Script ไม่มีการสนับสนุนเลขซับซ้อนอย่างในตัว, จำเป็นต้องมีการดำเนินการฟังก์ชันที่กำหนดเอง ด้านล่างนี้เป็นโครงสร้างพื้นฐานสำหรับการจัดการกับเลขซับซ้อน รวมถึงการบวก, การลบ, และการคูณ

```javascript
// กำหนด constructor สำหรับเลขซับซ้อน
function Complex(real, imag) {
  this.real = real;
  this.imag = imag;
}

// วิธีการสำหรับการบวกเลขซับซ้อนสองตัว
Complex.prototype.add = function(other) {
  return new Complex(this.real + other.real, this.imag + other.imag);
};

// วิธีการสำหรับการลบเลขซับซ้อนสองตัว
Complex.prototype.subtract = function(other) {
  return new Complex(this.real - other.real, this.imag - other.imag);
};

// วิธีการสำหรับการคูณเลขซับซ้อนสองตัว
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real
  );
};

// ตัวอย่างการใช้งาน
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// บวกเลขซับซ้อนสองตัว
var sum = num1.add(num2);
console.log(`ผลรวม: ${sum.real} + ${sum.imag}i`); // ผลรวม: 4 + 6i

// ลบเลขซับซ้อนสองตัว
var difference = num1.subtract(num2);
console.log(`ผลต่าง: ${difference.real} + ${difference.imag}i`); // ผลต่าง: 2 + 2i

// คูณเลขซับซ้อนสองตัว
var product = num1.multiply(num2);
console.log(`ผลคูณ: ${product.real} + ${product.imag}i`); // ผลคูณ: -5 + 10i
```

## รายละเอียดเพิ่มเติม:
แนวคิดของเลขซับซ้อนมีต้นกำเนิดตั้งแต่ศตวรรษที่ 16, แต่เป็นผลงานของนักคณิตศาสตร์อย่างออยเลอร์และเกาส์ที่ทำให้เลขซับซ้อนมีที่ยืนในวิชาคณิตศาสตร์แน่นหนา แม้จะมีประโยชน์, เลขซับซ้อนไม่ได้รับการสนับสนุนโดยตรงใน JavaScript หรือ Google Apps Script การขาดการสนับสนุนโดยตรงหมายความว่าการดำเนินการกับเลขซับซ้อนต้องถูกดำเนินการด้วยตนเองตามที่แสดงไว้ แม้ว่านี่จะเป็นโอกาสที่ดีในการเรียนรู้และมีฟังก์ชันที่เพียงพอสำหรับความต้องการพื้นฐาน, สำหรับงานคำนวณที่หนักหน่วงที่ต้องใช้เลขซับซ้อน, อาจพิจารณาใช้สภาพแวดล้อมการเขียนโปรแกรมอื่นที่เหมาะสมกว่ากับการคำนวณทางคณิตศาสตร์่, เช่น Python กับ NumPy, ซึ่งมีการดำเนินการที่ปรับแต่งได้อย่างดีและให้การสนับสนุนตามธรรมชาติสำหรับการจัดการกับเลขซับซ้อน อย่างไรก็ตาม, การเข้าใจและดำเนินการพื้นฐานใน Google Apps Script เป็นการฝึกฝนที่มีประโยชน์สำหรับผู้ที่ต้องการขยายทักษะการเขียนโปรแกรมของตนและนำไปใช้ในบริบทที่หลากหลาย.
