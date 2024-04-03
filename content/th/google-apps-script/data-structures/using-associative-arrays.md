---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:31.961177-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Google Apps\
  \ Script, \u0E04\u0E38\u0E13\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E41\u0E25\u0E30\u0E08\
  \u0E31\u0E14\u0E01\u0E32\u0E23\u0E41\u0E2D\u0E40\u0E23\u0E22\u0E4C\u0E40\u0E0A\u0E37\
  \u0E48\u0E2D\u0E21\u0E42\u0E22\u0E07 (objects) \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\
  \u0E27\u0E07\u0E40\u0E25\u0E47\u0E1A\u0E1B\u0E35\u0E01\u0E01\u0E32 `{}`, \u0E42\u0E14\
  \u0E22\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E39\u0E48\u0E04\u0E35\u0E22\u0E4C-\u0E04\
  \u0E48\u0E32\u0E20\u0E32\u0E22\u0E43\u0E19 \u0E04\u0E35\u0E22\u0E4C\u0E40\u0E1B\u0E47\
  \u0E19\u0E15\u0E31\u0E27\u0E23\u0E30\u0E1A\u0E38\u0E17\u0E35\u0E48\u0E44\u0E21\u0E48\
  \u0E0B\u0E49\u0E33\u0E01\u0E31\u0E19\u2026"
lastmod: '2024-03-17T21:57:55.708583-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Google Apps Script, \u0E04\u0E38\u0E13\u0E2A\u0E23\u0E49\u0E32\
  \u0E07\u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E41\u0E2D\u0E40\u0E23\
  \u0E22\u0E4C\u0E40\u0E0A\u0E37\u0E48\u0E2D\u0E21\u0E42\u0E22\u0E07 (objects) \u0E42\
  \u0E14\u0E22\u0E43\u0E0A\u0E49\u0E27\u0E07\u0E40\u0E25\u0E47\u0E1A\u0E1B\u0E35\u0E01\
  \u0E01\u0E32 `{}`, \u0E42\u0E14\u0E22\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E39\u0E48\
  \u0E04\u0E35\u0E22\u0E4C-\u0E04\u0E48\u0E32\u0E20\u0E32\u0E22\u0E43\u0E19 \u0E04\
  \u0E35\u0E22\u0E4C\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E23\u0E30\u0E1A\u0E38\
  \u0E17\u0E35\u0E48\u0E44\u0E21\u0E48\u0E0B\u0E49\u0E33\u0E01\u0E31\u0E19 \u0E41\u0E25\
  \u0E30\u0E04\u0E48\u0E32\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E40\u0E1B\u0E47\u0E19\
  \u0E2D\u0E30\u0E44\u0E23\u0E01\u0E47\u0E44\u0E14\u0E49\u0E15\u0E31\u0E49\u0E07\u0E41\
  \u0E15\u0E48\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E41\u0E25\u0E30\u0E15\
  \u0E31\u0E27\u0E40\u0E25\u0E02\u0E44\u0E1B\u0E08\u0E19\u0E16\u0E36\u0E07\u0E27\u0E31\
  \u0E15\u0E16\u0E38\u0E2B\u0E23\u0E37\u0E2D\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\
  \u0E19\u0E17\u0E35\u0E48\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19\u0E02\u0E36\u0E49\
  \u0E19 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E41\u0E2D\u0E40\u0E23\u0E22\u0E4C\u0E2A\
  \u0E21\u0E32\u0E0A\u0E34\u0E01"
weight: 15
---

## วิธีการ:
ใน Google Apps Script, คุณสร้างและจัดการแอเรย์เชื่อมโยง (objects) โดยใช้วงเล็บปีกกา `{}`, โดยกำหนดคู่คีย์-ค่าภายใน คีย์เป็นตัวระบุที่ไม่ซ้ำกัน และค่าสามารถเป็นอะไรก็ได้ตั้งแต่ตัวอักษรและตัวเลขไปจนถึงวัตถุหรือฟังก์ชันที่ซับซ้อนขึ้น นี่คือตัวอย่างพื้นฐาน:

```javascript
function createAssociativeArray() {
  var user = {
    name: "John Doe",
    age: 30,
    email: "johndoe@example.com"
  };

  // เข้าถึงค่า
  Logger.log(user.name); // แสดงออกมา: John Doe
  Logger.log(user["email"]); // แสดงออกมา: johndoe@example.com

  // เพิ่มคู่คีย์-ค่าใหม่
  user.title = "Software Developer";
  user["country"] = "USA";

  Logger.log(user.title); // แสดงออกมา: Software Developer

  // การวนซ้ำผ่านคู่คีย์-ค่า
  for (var key in user) {
    Logger.log(key + ': ' + user[key]);
  }
}
```

ผลลัพธ์ตัวอย่างสำหรับส่วนการวนซ้ำอาจจะเป็นแบบนี้:
```
name: John Doe
age: 30
email: johndoe@example.com
title: Software Developer
country: USA
```

สังเกตว่าคุณสามารถใช้ทั้ง dot notation และ bracket notation เพื่อเข้าถึงและตั้งค่าคุณสมบัติ Bracket notation เป็นประโยชน์โดยเฉพาะเมื่อทำงานกับคีย์ที่ถูกตัดสินใจอย่างไดนามิกหรือมีอักขระที่ไม่อนุญาตในตัวระบุ

## ลงลึก
แอเรย์เชื่อมโยงในรูปแบบของวัตถุได้เป็นหัวมุมหลักของ JavaScript, และตามนั้น Google Apps Script, ซึ่งสะท้อนถึงกลไกการถ่ายทอดต้นแบบของมัน ไม่เหมือนกับภาษาที่มีแอเรย์เชื่อมโยงแบบดั้งเดิมหรือ dictionaries (เช่น dict ของ Python), วัตถุของ Google Apps Script ให้วิธีการที่ยืดหยุ่นและมีพลังในการโครงสร้างข้อมูล, ได้รับประโยชน์จากธรรมชาติที่เป็นไดนามิกของ JavaScript

อย่างไรก็ตาม, สำคัญที่จะบันทึกว่า ข้อกำหนดของ ECMAScript 2015 ได้นำเสนอวัตถุ `Map` และ `Set`, ซึ่งให้การจัดการกลุ่มเชื่อมโยงที่ตรงไปตรงมามากขึ้นด้วยประโยชน์บางอย่างเหนือวัตถุ, เช่น การรักษาลำดับการแทรกและประสิทธิภาพที่ดีกว่าสำหรับชุดข้อมูลขนาดใหญ่ ในขณะที่ Google Apps Script ยังรองรับเหล่านี้, การเลือกใช้วัตถุหรือโครงสร้าง `Map`/`Set` ใหม่ขึ้นอยู่กับความต้องการและการพิจารณาประสิทธิภาพเฉพาะ สำหรับงานแอเรย์เชื่อมโยงส่วนใหญ่, การปฏิบัติตามโครงสร้างวัตถุแบบดั้งเดิมให้วิธีการที่คุ้นเคยและหลากหลาย แต่การตรวจสอบทางเลือกใหม่ๆ จะเป็นการดีเมื่อความซับซ้อนของสคริปต์ของคุณเพิ่มขึ้น
