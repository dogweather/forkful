---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:12.552863-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E48\u0E2D\u0E19\
  \u0E2D\u0E37\u0E48\u0E19 \u0E04\u0E38\u0E13\u0E08\u0E30\u0E15\u0E49\u0E2D\u0E07\u0E43\
  \u0E0A\u0E49\u0E15\u0E31\u0E27\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C TOML `@iarna/toml` \u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\
  \u0E40\u0E25\u0E37\u0E2D\u0E01\u0E22\u0E2D\u0E14\u0E19\u0E34\u0E22\u0E21 \u0E15\u0E34\
  \u0E14\u0E15\u0E31\u0E49\u0E07\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49 npm: `npm install\
  \ @iarna/toml --save`\u2026"
lastmod: '2024-03-17T21:57:55.968641-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E48\u0E2D\u0E19\u0E2D\u0E37\u0E48\u0E19 \u0E04\u0E38\u0E13\u0E08\
  \u0E30\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E15\u0E31\u0E27\u0E41\u0E22\u0E01\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C TOML `@iarna/toml` \u0E40\
  \u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E22\u0E2D\u0E14\
  \u0E19\u0E34\u0E22\u0E21 \u0E15\u0E34\u0E14\u0E15\u0E31\u0E49\u0E07\u0E42\u0E14\u0E22\
  \u0E43\u0E0A\u0E49 npm."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## วิธีการ:
ก่อนอื่น คุณจะต้องใช้ตัวแยกวิเคราะห์ TOML `@iarna/toml` เป็นตัวเลือกยอดนิยม ติดตั้งโดยใช้ npm: `npm install @iarna/toml --save` นี่คือวิธีที่คุณอ่านไฟล์ TOML และแยกวิเคราะห์เป็นอ็อบเจกต์ JavaScript:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
หาก `config.toml` มีเนื้อหา:
```
[server]
port = 8080
```
ผลลัพธ์จะเป็น:
```
{ server: { port: 8080 } }
```
และ, การเขียนลงไฟล์ TOML ก็ง่ายไม่แพ้กัน:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
การรันโค้ดนี้จะเขียนอ็อบเจกต์ลงใน `config.toml` ในรูปแบบ TOML

## ลงลึก
TOML ถูกสร้างโดย Tom Preston-Werner ผู้ร่วมก่อตั้ง GitHub ในปี 2013 เป็นการตอบสนองต่อข้อจำกัดที่เขาเห็นในรูปแบบอื่นๆ เช่น INI หรือ YAML มันถูกออกแบบมาเพื่อการไม่กำกวมและสามารถแยกวิเคราะห์ไปยังโครงสร้างข้อมูลได้ง่าย ด้วยเหตุนี้จึงเป็นที่ชื่นชอบสำหรับไฟล์การตั้งค่า ตัวเลือกทางเลือกเช่น JSON ขาดความสามารถในการใส่คอมเมนต์ ในขณะที่ YAML ซับซ้อนกว่า TOML เด่นในความเรียบง่ายและความสามารถในการแสดงข้อมูลที่ซับซ้อนได้อย่างชัดเจน

ภายใน, เมื่อคุณแยกวิเคราะห์ TOML ใน TypeScript คุณกำลังแปลงข้อมูลตัวอักษรเป็นรูปแบบที่โครงสร้างได้ซึ่งภาษาสามารถจัดการได้ ซึ่งรวมถึงการจัดลำดับคำ (การแปลงข้อความดิบเป็นโทเค็น) และการแยกวิเคราะห์ (การสร้างโครงสร้างข้อมูลภายใน) `@iarna/toml` จัดการทั้งสองอย่างได้อย่างราบรื่น การสนับสนุนอีโมจิเป็นลูกเล่นที่น่าสนใจ โชว์ว่า TOML มีการออกแบบที่เน้นผู้ใช้

## ดูเพิ่มเติม
- ข้อกำหนด TOML อย่างเป็นทางการ: https://toml.io/en/
- แพ็คเกจ `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
- เปรียบเทียบระหว่าง TOML, YAML, และ JSON: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
