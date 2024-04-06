---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:34.399182-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Google Apps\
  \ Script, \u0E01\u0E32\u0E23\u0E17\u0E33 Logging \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E17\u0E33\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E27\u0E34\u0E18\
  \u0E35\u0E15\u0E48\u0E32\u0E07\u0E46 \u0E40\u0E0A\u0E48\u0E19 \u0E04\u0E25\u0E32\
  \u0E2A `Logger` \u0E41\u0E25\u0E30 `console.log()` \u0E04\u0E25\u0E32\u0E2A Logger\
  \ \u0E40\u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\u0E14\u0E31\u0E49\u0E07\u0E40\
  \u0E14\u0E34\u0E21\u2026"
lastmod: '2024-04-05T21:54:01.107620-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Google Apps Script, \u0E01\u0E32\u0E23\u0E17\u0E33 Logging\
  \ \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\u0E14\u0E49\u0E42\u0E14\
  \u0E22\u0E43\u0E0A\u0E49\u0E27\u0E34\u0E18\u0E35\u0E15\u0E48\u0E32\u0E07\u0E46 \u0E40\
  \u0E0A\u0E48\u0E19 \u0E04\u0E25\u0E32\u0E2A `Logger` \u0E41\u0E25\u0E30 `console.log()`\
  \ \u0E04\u0E25\u0E32\u0E2A Logger \u0E40\u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\u0E35\
  \u0E14\u0E31\u0E49\u0E07\u0E40\u0E14\u0E34\u0E21 \u0E17\u0E35\u0E48\u0E40\u0E2B\u0E21\
  \u0E32\u0E30\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23 debug \u0E41\u0E25\
  \u0E30\u0E01\u0E32\u0E23\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E35\u0E48\u0E07\u0E48\
  \u0E32\u0E22 \u0E15\u0E31\u0E49\u0E07\u0E41\u0E15\u0E48\u0E01\u0E32\u0E23\u0E2D\u0E31\
  \u0E1B\u0E40\u0E14\u0E15\u0E25\u0E48\u0E32\u0E2A\u0E38\u0E14, `console.log()` \u0E21\
  \u0E35\u0E04\u0E27\u0E32\u0E21\u0E22\u0E37\u0E14\u0E2B\u0E22\u0E38\u0E48\u0E19\u0E41\
  \u0E25\u0E30\u0E01\u0E32\u0E23\u0E1C\u0E2A\u0E32\u0E19\u0E23\u0E27\u0E21\u0E01\u0E31\
  \u0E1A Stackdriver Logging \u0E21\u0E32\u0E01\u0E02\u0E36\u0E49\u0E19 \u0E43\u0E2B\
  \u0E49\u0E42\u0E0B\u0E25\u0E39\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E02\u0E47\
  \u0E07\u0E41\u0E01\u0E23\u0E48\u0E07\u0E02\u0E36\u0E49\u0E19\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A Apps Scripts\
  \ \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E43\u0E19 Google Cloud Platform **\u0E01\
  \u0E32\u0E23\u0E43\u0E0A\u0E49 Logger:**."
title: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E25\u0E47\u0E2D\u0E01"
weight: 17
---

## วิธีการ:
ใน Google Apps Script, การทำ Logging สามารถทำได้โดยใช้วิธีต่างๆ เช่น คลาส `Logger` และ `console.log()` คลาส Logger เป็นวิธีดั้งเดิม ที่เหมาะสำหรับการ debug และการพัฒนาที่ง่าย ตั้งแต่การอัปเดตล่าสุด, `console.log()` มีความยืดหยุ่นและการผสานรวมกับ Stackdriver Logging มากขึ้น ให้โซลูชันที่แข็งแกร่งขึ้นสำหรับการตรวจสอบ Apps Scripts ของคุณใน Google Cloud Platform

**การใช้ Logger:**

```javascript
function logSample() {
  Logger.log('This is a simple log message');
  
  var value = 5;
  Logger.log('The value is: %s', value); // การจัดรูปแบบสตริง
}

// การดู log:
// 1. รันฟังก์ชัน logSample
// 2. เลือกเมนู View -> Logs
```

**ตัวอย่างของ Logger Output:**

```
[22-04-20 10:00:00:000 PDT] This is a simple log message
[22-04-20 10:00:00:001 PDT] The value is: 5
```

**การใช้ console.log():**

```javascript
function consoleLogSample() {
  console.log('This message goes to Stackdriver Logging');
  const obj = {name: 'Jane', role: 'Developer'};
  console.info('Logging an object:', obj);
}

// Logs สามารถดูได้ในแดชบอร์ด Google Cloud Platform (GCP) ภายใต้ Stackdriver Logging
```

**ตัวอย่างของ console.log() Output:**

```
This message goes to Stackdriver Logging
Logging an object: {name: "Jane", role: "Developer"}
```

โดยการเปลี่ยนไปใช้ `console.log()` สำหรับแอปพลิเคชันที่ซับซ้อน นักพัฒนาสามารถวิเคราะห์และแยกวิเคราะห์บันทึกโดยใช้ตัวกรองและเครื่องมือที่ทรงพลังที่ GCP ให้มา ซึ่งไม่ง่ายเท่ากับการใช้คลาส Logger ดั้งเดิม

## การทำความเข้าใจลึกซึ้ง:
การ Logging ใน Google Apps Script ได้พัฒนาขึ้นอย่างมาก แต่เดิมคลาส `Logger` เป็นวิธีหลักที่นักพัฒนาใช้ในการ debug สคริปต์ของตน เป็นวิธีที่เรียบง่ายและเพียงพอสำหรับสคริปต์พื้นฐาน แต่มันขาดความสามารถที่จำเป็นสำหรับแอปพลิเคชันระบบคลาวด์สมัยใหม่ เช่น การค้นหาบันทึกหรือวิเคราะห์แนวโน้มของบันทึกตามเวลา

การนำเสนอ `console.log()` ช่วยเติมช่องว่างนี้โดยการผสานรวมการ Logging ของ Google Apps Script กับ Stackdriver Logging ของ Google Cloud (ตอนนี้เรียกว่า Operations Suite) ให้แพลตฟอร์มกลางสำหรับการบันทึก การตรวจสอบ และการดีบักแอปพลิเคชัน ไม่เพียงแต่ช่วยให้การบันทึกระดับสูงได้ แต่ยังเปิดใช้งานฟีเจอร์การจัดการบันทึกขั้นสูง เช่น มาตรวัดบนพื้นฐานของบันทึก การวิเคราะห์บันทึกเป็นเรียลไทม์ และการผสานรวมกับบริการของ Google Cloud อื่นๆ

ในขณะที่ `Logger` ยังคงมีวัตถุประสงค์สำหรับการ debug และการบันทึกในสคริปต์ขนาดเล็ก การพัฒนาการใช้ `console.log()` สะท้อนถึงการเปลี่ยนแปลงที่กว้างขึ้นในการพัฒนาแอปพลิเคชันที่สามารถขยายขนาดได้และเป็นเนทีฟคลาวด์ มันเน้นความมุ่งมั่นของ Google ในการให้เครื่องมือแก่นักพัฒนาที่ตอบสนองต่อความซับซ้อนและขนาดของแอปพลิเคชันในปัจจุบัน อย่างไรก็ตาม มือใหม่ควรระวังถึงการเรียนรู้ที่ลึกขึ้นเล็กน้อยและความจำเป็นในการทำความคุ้นเคยกับแนวคิดของ Google Cloud Platform ถึงแม้ว่าการเปลี่ยนแปลงนี้จะเป็นประโยชน์สำหรับนักพัฒนาที่ต้องการใช้ประโยชน์จากความสามารถของคลาวด์อย่างเต็มที่ การเป็นหนึ่งกับบริการคลาวด์เป็นส่วนหนึ่งของแนวโน้มทั่วไปในการพัฒนาซอฟต์แวร์ ซึ่งเน้นถึงความสำคัญของกลไกการบันทึกที่แข็งแกร่งและสามารถขยายขนาดได้ในยุคของคลาวด์คอมพิวติ้ง
