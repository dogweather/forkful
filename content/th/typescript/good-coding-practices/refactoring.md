---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:01.243861-06:00
description: "\u0E01\u0E32\u0E23 Refactoring \u0E04\u0E37\u0E2D\u0E01\u0E23\u0E30\u0E1A\
  \u0E27\u0E19\u0E01\u0E32\u0E23\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\
  \u0E43\u0E2B\u0E21\u0E48\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E04\u0E2D\u0E21\
  \u0E1E\u0E34\u0E27\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\
  \u0E22\u0E39\u0E48\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\
  \u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\
  \u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01 \u0E40\u0E2B\u0E25\u0E48\u0E32\u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E42\u0E04\u0E49\u0E14\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E02\u0E36\u0E49\u0E19\u2026"
lastmod: '2024-03-17T21:57:55.953954-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23 Refactoring \u0E04\u0E37\u0E2D\u0E01\u0E23\u0E30\u0E1A\
  \u0E27\u0E19\u0E01\u0E32\u0E23\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\
  \u0E43\u0E2B\u0E21\u0E48\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E04\u0E2D\u0E21\
  \u0E1E\u0E34\u0E27\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\
  \u0E22\u0E39\u0E48\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\
  \u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\
  \u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01 \u0E40\u0E2B\u0E25\u0E48\u0E32\u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E43\u0E2B\u0E49\
  \u0E42\u0E04\u0E49\u0E14\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E02\u0E36\u0E49\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การ Refactoring คือกระบวนการโครงสร้างใหม่ของโค้ดคอมพิวเตอร์ที่มีอยู่โดยไม่เปลี่ยนแปลงพฤติกรรมภายนอก เหล่าโปรแกรมเมอร์ทำเช่นนี้เพื่อทำให้โค้ดสะอาดขึ้น ง่ายต่อการบำรุงรักษา และลดความซับซ้อน ซึ่งทำให้ง่ายต่อการเข้าใจสำหรับใครก็ตามที่ดำน้ำเข้ามาใหม่

## วิธีการ:
พิจารณาฟังก์ชัน TypeScript ที่เคยเห็นวันที่ดีกว่านี้ - มันเป็นสิ่งที่ยุ่งเหยิงเล็กน้อย และอาจใช้ความรักและการดูแลบ้าง:

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
ถ้ามีการ Refactored, อาจจะดูเหมือนนี้:

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

ตัวอย่างที่สองนั้นแข็งแกร่งกว่า เนื่องจากใช้ระบบประเภทของ TypeScript พร้อมกับ `interface` เพื่อหลีกเลี่ยงข้อผิดพลาดที่อาจเกิดขึ้นในระหว่างการรันและปรับปรุงความสามารถในการอ่าน

## การศึกษาลึกล้ำ
การ Refactoring ไม่ใช่แนวคิดสมัยใหม่; มันพัฒนาไปพร้อมกับการเขียนโปรแกรม กลายเป็นรูปแบบที่เป็นทางการมากขึ้นด้วยการปล่อยหนังสือ "Refactoring: Improving the Design of Existing Code" ของ Martin Fowler ในปี 1999 มันมีความสำคัญในสภาพแวดล้อมการพัฒนา Agile ช่วยในการเปลี่ยนแปลงโค้ดที่ปรับตัวได้ ทางเลือกบางอย่างสำหรับการ Refactoring แบบด้วยตนเองรวมถึงเครื่องมืออัตโนมัติเช่น TSLint หรือเซิร์ฟเวอร์ภาษาของ TypeScript เองที่สามารถแนะนำหรือแม้กระทั่งทำงาน Refactoring บางอย่างให้คุณได้ รายละเอียดการดำเนินการโดยปกติจะเกี่ยวข้องกับการรู้จำ "กลิ่นโค้ด" เช่น โค้ดที่ซ้ำซ้อน วิธีการที่ยาว หรือคลาสใหญ่ และใช้รูปแบบในการรักษา เช่น การแยกฟังก์ชัน การย้ายไปยังคลาสที่เหมาะสมยิ่งขึ้น หรือการใช้โครงสร้างที่ง่ายกว่า รูปแบบเหล่านี้เป็นกุญแจสำคัญในการเข้าใจวิธีการและเหตุผลของการ Refactoring

## ดูเพิ่มเติม
- [หนังสือ "Refactoring: Improving the Design of Existing Code" โดย Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [TSLint สำหรับการวิเคราะห์โค้ดเชิงสถิต](https://palantir.github.io/tslint/)
- [การเข้าใจกลิ่นโค้ด](https://refactoring.guru/refactoring/smells)
