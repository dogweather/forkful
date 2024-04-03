---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:24.293814-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: ."
lastmod: '2024-03-17T21:57:55.945019-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\u0E04\u0E23\u0E07\u0E01\
  \u0E32\u0E23\u0E43\u0E2B\u0E21\u0E48"
weight: 1
---

## วิธีการ:
```TypeScript
// ขั้นตอนที่ 1: ติดตั้ง TypeScript แบบทั่วโลก (หากยังไม่ได้ติดตั้ง)
npm install -g typescript

// ขั้นตอนที่ 2: สร้างไดเรกทอรีใหม่สำหรับโปรเจกต์ของคุณ
mkdir my-new-project
cd my-new-project

// ขั้นตอนที่ 3: เริ่มต้นโปรเจกต์ node ใหม่
npm init -y

// ขั้นตอนที่ 4: ติดตั้ง TypeScript ในโปรเจกต์ของคุณ
npm install typescript --save-dev

// ขั้นตอนที่ 5: เริ่มต้นโปรเจกต์ TypeScript เพื่อสร้าง tsconfig.json
tsc --init

// ตัวอย่างผลลัพธ์ tsconfig.json (ละบางส่วนเพื่อความกระชับ)
{
  "compilerOptions": {
    "target": "es5",
    "module": "commonjs",
    "strict": true,
    ...
  }
}

// ขั้นตอนที่ 6: สร้างไฟล์ TypeScript ง่ายๆ 'hello.ts'
echo 'console.log("Hello, TypeScript!");' > hello.ts

// ขั้นตอนที่ 7: คอมไพล์ไฟล์ TypeScript และรัน
tsc hello.ts
node hello.js

// ตัวอย่างผลลัพธ์
Hello, TypeScript!
```

## การศึกษาลึก
TypeScript, เป็นซุปเปอร์เซ็ตของ JavaScript, พัฒนาโดย Microsoft และเปิดตัวครั้งแรกในเดือนตุลาคมปี 2012 TypeScript เพิ่ม static types ให้กับ JavaScript ซึ่งสามารถช่วยจับภาพพลาดก่อนที่จะถึงเวลารันไทม์ได้ และสนับสนุนคุณสมบัติ IDE เช่น การนำทางโค้ดและการปรับโครงสร้างใหม่

แม้ว่าวิธีการข้างต้นใช้ npm (Node Package Manager), แต่ยังมีวิธีอื่นในการจัดการโปรเจกต์ TypeScript เช่น Yarn หรือ pnpm ทางเลือกในการเริ่มต้นโปรเจกต์ TypeScript ได้แก่ การสร้างโปรเจกต์โดยใช้ชุดเริ่มต้นหรือการโคลน boilerplate จากที่เก็บเช่น GitHub

`tsconfig.json` เป็นเรื่องสำคัญ; มันเป็นตัวบอกวิธีที่ TypeScript Compiler (tsc) แปลงโค้ด TypeScript ของคุณเป็น JavaScript การปรับตัวเลือกคอมไพเลอร์ช่วยให้คุณเป้าหมายไปที่เวอร์ชัน ECMAScript ต่างๆ, ระบบโมดูล, และอื่นๆ, ตามความต้องการของโปรเจกต์

## ดูเพิ่มเติม
- TypeScript เอกสารอย่างเป็นทางการ: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- TypeScript GitHub Repo: [https://github.com/microsoft/TypeScript](https://github.com/microsoft/TypeScript)
- TypeScript Deep Dive: [https://basarat.gitbook.io/typescript/](https://basarat.gitbook.io/typescript/)
- Awesome TypeScript: [https://github.com/dzharii/awesome-typescript](https://github.com/dzharii/awesome-typescript)
