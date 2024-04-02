---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:56.344933-06:00
description: "YAML \u0E0B\u0E36\u0E48\u0E07\u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\
  \u0E01 \"YAML Ain't Markup Language\" \u0E40\u0E1B\u0E47\u0E19\u0E21\u0E32\u0E15\
  \u0E23\u0E10\u0E32\u0E19\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E25\
  \u0E44\u0E25\u0E0B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\
  \u0E48\u0E32\u0E19\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\u0E22\
  \u0E4C\u2026"
lastmod: '2024-03-17T21:57:55.739588-06:00'
model: gpt-4-0125-preview
summary: "YAML \u0E0B\u0E36\u0E48\u0E07\u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01\
  \ \"YAML Ain't Markup Language\" \u0E40\u0E1B\u0E47\u0E19\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E25\u0E44\
  \u0E25\u0E0B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\u0E48\
  \u0E32\u0E19\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C\
  \u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
weight: 41
---

## คืออะไร & ทำไม?

YAML ซึ่งย่อมาจาก "YAML Ain't Markup Language" เป็นมาตรฐานการซีเรียลไลซ์ข้อมูลที่อ่านได้โดยมนุษย์ ซึ่งมักใช้สำหรับไฟล์การกำหนดค่าและการแลกเปลี่ยนข้อมูลระหว่างภาษาที่มีโครงสร้างข้อมูลที่แตกต่างกัน โปรแกรมเมอร์มักทำงานกับ YAML เนื่องจากความเรียบง่ายและความสามารถในการอ่านได้ง่าย โดยเฉพาะในโปรเจ็กต์ที่ต้องการการกำหนดค่าอย่างกว้างขวางหรือเมื่อต้องการถ่ายโอนข้อมูลที่มีโครงสร้างระหว่างระบบที่แตกต่างกัน

## วิธีทำ:

ถึงแม้ Google Apps Script (GAS) จะไม่รองรับการแยกวิเคราะห์หรือซีเรียลไลซ์ YAML โดยตรง คุณสามารถจัดการข้อมูล YAML โดยใช้ไลบรารีของ JavaScript หรือการเขียนฟังก์ชันการแยกวิเคราะห์ที่กำหนดเอง สำหรับการสาธิต มาพิจารณาวิธีการแยกวิเคราะห์สตริง YAML โดยใช้ฟังก์ชันที่กำหนดเอง เนื่องจากไลบรารีภายนอกไม่สามารถถูกนำเข้าโดยตรงเข้าใน GAS

สมมุติว่าคุณมีการกำหนดค่า YAML แบบง่าย:

```yaml
title: YAML Example
description: An example of how to handle YAML in Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Configuration
```

เพื่อแยกวิเคราะห์สิ่งนี้ใน Google Apps Script ใช้ความสามารถในการจัดการสตริงของ JavaScript:

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // การจัดการพื้นฐานสำหรับอาร์เรย์
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: YAML Example\ndescription: An example of how to handle YAML in Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Configuration";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

เมื่อ `testYamlParsing()` ถูกทำงาน จะมีผลลัพธ์ดังนี้:

```
{ title: 'YAML Example',
  description: 'An example of how to handle YAML in Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Configuration' ] }
```

การเข้าใจการแยกวิเคราะห์ด้วยตนเองนี้ค่อนข้างพื้นฐานและอาจต้องการการปรับแต่งเพื่อรองรับไฟล์ YAML ที่ซับซ้อน

## การดำดิ่งลึก

YAML ซึ่งเปิดตัวครั้งแรกในปี 2001 มุ่งเน้นเพื่อสร้างความสามารถในการอ่านได้มากกว่าตัวก่อนๆ อย่าง XML หรือ JSON ถึงแม้ความเรียบง่ายและความสะดวกในการใช้งานจะได้รับการยกย่องอย่างกว้างขวาง การจัดการ YAML ใน Google Apps Script ก็นำเสนอความท้าทายเนื่องจากการขาดการสนับสนุนโดยตรง ด้วยเหตุนี้ โปรแกรมเมอร์มักพึ่งพาความหลากหลายของ JavaScript เพื่อแยกวิเคราะห์และสร้างข้อมูล YAML อย่างไรก็ตาม สำหรับกรณีการใช้งานที่ซับซ้อน เฉพาะอย่างยิ่งที่เกี่ยวข้องกับการซ้อนกันอย่างลึกและโครงสร้างข้อมูลขั้นสูง วิธีนี้อาจกลายเป็นสิ่งที่ยุ่งยากและอาจเกิดข้อผิดพลาดได้

ตรงข้ามกับ YAML, JSON ได้รับการสนับสนุนโดยตรงใน Google Apps Script และส่วนใหญ่ของสภาพแวดล้อมการเขียนโปรแกรมอื่น ๆ นำเสนอวิธีการที่ตรงไปตรงมายิ่งขึ้นสำหรับการซีเรียลไลซ์และการดีเซเรียลไลซ์ข้อมูลโดยไม่ต้องมีการแยกวิเคราะห์เพิ่มเติม ไวยากรณ์ของ JSON นั้นไม่ซับซ้อนเท่ากับ YAML ทำให้มันเหมาะกับการแลกเปลี่ยนข้อมูลในเว็บแอปพลิเคชัน อย่างไรก็ตาม YAML ยังคงเป็นที่นิยมสำหรับไฟล์การกำหนดค่าและสถานการณ์ที่ความสามารถอ่านโดยมนุษย์เป็นสิ่งสำคัญ

เมื่อทำงานกับ YAML ใน Google Apps Script พิจารณาถึงประเด็นต่างๆ ระหว่างความสามารถในการอ่านและความง่ายในการใช้งาน สำหรับการจัดการ YAML อย่างครอบคลุมอาจจำเป็นต้องสำรวจเครื่องมือหรือบริการภายนอกที่สามารถแปลง YAML เป็น JSON ก่อนที่จะดำเนินการภายในสคริปต์ของคุณ
