---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:47.877405-06:00
description: "\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E23\u0E34\u0E48\u0E21\u0E43\u0E0A\
  \u0E49\u0E40\u0E14\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\u0E01\u0E31\u0E1A\
  \ TypeScript \u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E15\u0E49\
  \u0E2D\u0E07\u0E01\u0E32\u0E23\u0E04\u0E37\u0E2D IDE \u0E17\u0E35\u0E48\u0E23\u0E2D\
  \u0E07\u0E23\u0E31\u0E1A (\u0E40\u0E0A\u0E48\u0E19 Visual Studio Code) \u0E41\u0E25\
  \u0E30\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32 `launch.json`\u2026"
lastmod: '2024-03-17T21:57:55.949878-06:00'
model: gpt-4-0125-preview
summary: "\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E23\u0E34\u0E48\u0E21\u0E43\u0E0A\
  \u0E49\u0E40\u0E14\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\u0E01\u0E31\u0E1A\
  \ TypeScript \u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E15\u0E49\
  \u0E2D\u0E07\u0E01\u0E32\u0E23\u0E04\u0E37\u0E2D IDE \u0E17\u0E35\u0E48\u0E23\u0E2D\
  \u0E07\u0E23\u0E31\u0E1A (\u0E40\u0E0A\u0E48\u0E19 Visual Studio Code) \u0E41\u0E25\
  \u0E30\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32 `launch."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
weight: 35
---

## วิธีการ:
เพื่อเริ่มใช้เดบักเกอร์กับ TypeScript สิ่งที่คุณต้องการคือ IDE ที่รองรับ (เช่น Visual Studio Code) และการตั้งค่า `launch.json` นี่คือตัวอย่างง่ายๆสำหรับแอปพลิเคชัน Node.js:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Hello, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

เพื่อเดบักสิ่งนี้, สร้างไฟล์ `launch.json` ใต้โฟลเดอร์ `.vscode`:

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "Launch Program",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

จากนั้น, ตั้งจุดหยุดในฟังก์ชัน `greet` ของคุณโดยการคลิกที่ด้านซ้ายของหมายเลขบรรทัดใน IDE ของคุณ กด F5 เพื่อเริ่มการเดบัก และสังเกตแอปพลิเคชันของคุณหยุดที่จุดหยุด คุณสามารถเลื่อนเมาส์ไปที่ตัวแปร, ดูนิพจน์, และเดินทางผ่านโค้ดของคุณได้อย่างง่ายดาย

## ลงลึก
กลับไปในวันก่อนเมื่อสภาพแวดล้อมการพัฒนาแบบรวม (IDEs) ยังไม่ดีนักระบบการเดบักมักจะทำด้วยคำสั่งการพิมพ์ (หรือการเดบักด้วย `console.log`) มันทำงานได้ ประมาณนั้น แต่เหมือนกับการพยายามหาเข็มในกองหญ้าขณะที่ตามัว
 
เดบักเกอร์สมัยใหม่เหมือนเป็น Swiss Army knife สำหรับการตรวจสอบปัญหา ด้วยการพัฒนาของ TypeScript และ Node.js มีเดบักเกอร์หลายชนิดที่ใช้ได้ ตั้งแต่ตัวตรวจสอบแบบในตัวของ Node.js ไปจนถึงเครื่องมือพัฒนาเว็บสำหรับการเดบักทางด้านไคลเอนต์

ตัวตรวจสอบของ Node.js ทำงานโดยการติดกับแอปพลิเคชันที่กำลังรันอยู่; มันสื่อสารผ่าน Chrome DevTools Protocol ทำให้เบราว์เซอร์ Chrome ของคุณเป็นคอนโซลเดบักที่ทรงพลัง การรวมกันนี้ช่วยให้มีเซสชันการเดบักที่มีปฏิสัมพันธ์ทางภาพและละเอียดยิ่งขึ้นเมื่อเทียบกับการปฏิบัติการเดบักแบบระบบคอมมานด์ไลน์

## ดูเพิ่มเติม
สำหรับการอ่านเพิ่มเติมและเคล็ดลับโปร ตรวจสอบที่:

- [การเดบัก TypeScript ใน Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [คู่มือการเดบัก Node.js](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [เอกสารเครื่องมือพัฒนา Chrome](https://developers.google.com/web/tools/chrome-devtools)
