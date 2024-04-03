---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:18.425238-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: ."
lastmod: '2024-03-17T21:57:55.935779-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 3
---

## วิธีทำ:
```TypeScript
let greeting: string = "Hello";
let target: string = "World";
let message: string = greeting + ", " + target + "!"; // โดยใช้ operator +
console.log(message); // ผลลัพธ์: Hello, World!

let anotherMessage: string = `${greeting}, ${target}!`; // โดยใช้ template literals
console.log(anotherMessage); // ผลลัพธ์: Hello, World!
```

## ลงลึก
การต่อข้อความเป็นพื้นฐาน; มันเป็นสิ่งที่มีมาตั้งแต่ยุคแรกๆ ของการเขียนโปรแกรม ใน TypeScript, ซึ่งสร้างขึ้นมาบน JavaScript, เราได้พัฒนามาจากการดำเนินการกับข้อความที่ไม่สะดวกมาเป็น template literals ที่ลื่นไหลและสะดวกยิ่งขึ้น

ในอดีต, คุณต้องระวังเรื่องการผสมข้อความเพื่อไม่ให้ใช้หน่วยความจำมากเกินไปหรือทำให้เบราว์เซอร์ทำงานช้าลง แม้ว่าเครื่องจักรสมัยใหม่จะถูกปรับให้เหมาะสม, แต่ประสิทธิภาพก็ยังสำคัญในแอพขนาดใหญ่

มีทางเลือกอื่นๆ:
1. อาร์เรย์และ `.join()`: มีประโยชน์เมื่อคุณกำลังจัดการกับรายการข้อความ
2. รูปแบบของ StringBuilder: มีความเกี่ยวข้องมากขึ้นกับภาษาเช่น Java หรือ C# ซึ่งมีการประสิทธิภาพที่ดีขึ้น

ในการเป็นตัวนำมาใช้, TypeScript จะถูกคอมไพล์เป็น JavaScript ลึกลงไป, มันใช้ฟังก์ชันและการดำเนินการกับข้อความเดียวกันที่ JavaScript ให้มา

## ดูเพิ่มเติม
- คุณอาจต้องการตรวจสอบ Mozilla Developer Network [เอกสารข้อความ](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String) เพื่อดูวิธีการใช้งานข้อความอย่างลึกซึ้ง
- สำหรับคำถามเกี่ยวกับข้อความใน TypeScript โดยเฉพาะ, [เอกสารทางการของ TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string) เป็นแหล่งข้อมูลอ้างอิงที่ดี
