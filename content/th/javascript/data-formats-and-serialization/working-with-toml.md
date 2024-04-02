---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:00.365877-06:00
description: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious,\
  \ Minimal Language, \u0E01\u0E33\u0E2B\u0E19\u0E14\u0E27\u0E34\u0E18\u0E35\u0E01\
  \u0E32\u0E23\u0E08\u0E31\u0E14\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\
  \u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\
  \u0E0A\u0E49\u0E07\u0E32\u0E19 TOML \u0E40\u0E1E\u0E23\u0E32\u0E30\u0E21\u0E31\u0E19\
  \u0E2D\u0E48\u0E32\u0E19\u0E07\u0E48\u0E32\u0E22, \u0E40\u0E02\u0E35\u0E22\u0E19\
  \u0E07\u0E48\u0E32\u0E22\u2026"
lastmod: '2024-03-17T21:57:56.625284-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious, Minimal\
  \ Language, \u0E01\u0E33\u0E2B\u0E19\u0E14\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\
  \u0E08\u0E31\u0E14\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\
  \u0E25\u0E4C\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32 \u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E07\
  \u0E32\u0E19 TOML \u0E40\u0E1E\u0E23\u0E32\u0E30\u0E21\u0E31\u0E19\u0E2D\u0E48\u0E32\
  \u0E19\u0E07\u0E48\u0E32\u0E22, \u0E40\u0E02\u0E35\u0E22\u0E19\u0E07\u0E48\u0E32\
  \u0E22\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## อะไรและทำไม?
TOML ย่อมาจาก Tom's Obvious, Minimal Language, กำหนดวิธีการจัดโครงสร้างไฟล์การตั้งค่า โปรแกรมเมอร์ใช้งาน TOML เพราะมันอ่านง่าย, เขียนง่าย และสามารถจับคู่ได้เป็นตารางแฮช ทำให้เป็นตัวเลือกสำหรับการกำหนดค่า

## วิธีการ:
เพื่อทำงานกับ TOML ใน JavaScript, คุณจะต้องมีตัวแยกวิเคราะห์ เช่น `@iarna/toml` ก่อนอื่น, ติดตั้งมัน: `npm install @iarna/toml` จากนั้น, แยกวิเคราะห์สตริง TOML เป็นอ็อบเจ็กต์ JavaScript หรือสร้างสตริง TOML จากอ็อบเจ็กต์ JavaScript

```javascript
const toml = require('@iarna/toml');

// แยกวิเคราะห์สตริง TOML เป็นอ็อบเจ็กต์ JS
const tomlStr = `
title = "ตัวอย่าง TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// แปลงอ็อบเจ็กต์ JS เป็นสตริง TOML
const jsObject = {
  title: "ตัวอย่าง TOML",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## ลงลึก
TOML ถูกเปิดตัวครั้งแรกในปี 2013 โดย Tom Preston-Werner, ผู้ร่วมก่อตั้ง GitHub ได้ถูกออกแบบมาเพื่อแทนที่รูปแบบอื่นๆ เช่น INI โดยการเป็นมาตรฐานและง่ายต่อการแยกวิเคราะห์มากขึ้น JSON และ YAML เป็นทางเลือก แต่อาจซับซ้อนหรือยืดหยุ่นเกินไป TOML มีข้อได้เปรียบในการกำหนดค่าแบบคงที่ที่ต้องการรูปแบบที่ง่ายและชัดเจน การออกแบบของมันช่วยให้สามารถแมปง่ายๆ เข้ากับตารางแฮช โดยมีคีย์และค่าที่สอดคล้องกับชื่อและค่าของคุณสมบัติ สำหรับการนำไปใช้กว้างขวางยิ่งขึ้น คุณอาจต้องรวมเครื่องมือที่สามารถแปลงระหว่าง TOML และรูปแบบอื่นๆ เนื่องจากการสนับสนุนด้านระบบนิเวศที่หลากหลาย

## ดูเพิ่มเติม
- ที่เก็บ GitHub อย่างเป็นทางการของ TOML: https://github.com/toml-lang/toml
- TOML กับ YAML กับ JSON เปรียบเทียบ: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- npm แพคเกจ `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
