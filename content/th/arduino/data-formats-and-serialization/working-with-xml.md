---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:07.532374-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML\
  \ \u0E1A\u0E19 Arduino \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\
  \u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19\u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 XML \u0E0B\u0E36\u0E48\u0E07\u0E21\
  \u0E31\u0E01\u0E21\u0E32\u0E08\u0E32\u0E01 Web API \u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32 \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\
  \u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E23\u0E27\u0E21\
  \u0E01\u0E31\u0E1A\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E43\u0E0A\
  \u0E49 XML\u2026"
lastmod: '2024-03-17T21:57:56.504233-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E1A\
  \u0E19 Arduino \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\
  \u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19\u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 XML \u0E0B\u0E36\u0E48\u0E07\u0E21\u0E31\
  \u0E01\u0E21\u0E32\u0E08\u0E32\u0E01 Web API \u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E1F\
  \u0E25\u0E4C\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32 \u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\
  \u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E23\u0E27\u0E21\u0E01\
  \u0E31\u0E1A\u0E1A\u0E23\u0E34\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49\
  \ XML\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การทำงานกับ XML บน Arduino หมายถึงการแยกส่วนและจัดการข้อมูล XML ซึ่งมักมาจาก Web API หรือไฟล์การตั้งค่า โปรแกรมเมอร์ทำเช่นนี้เพื่อรวมกับบริการที่ใช้ XML สำหรับการแลกเปลี่ยนข้อมูล หรือเพื่อจัดเก็บข้อมูลในรูปแบบที่มีโครงสร้างและสามารถอ่านได้โดยมนุษย์

## วิธีการ:
เราจะใช้ไลบรารี `XMLWriter` เพื่อสร้าง XML และไลบรารี `tinyxml2` เพื่อแยกส่วนมัน ติดตั้งไลบรารีเหล่านี้ผ่าน Library Manager ใน Arduino IDE ของคุณ

การสร้างเอกสาร XML:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // ใช้ Serial เพื่อแสดงผล
  
  xml.header();
  xml.tag("greeting").tag("text").text("Hello, world!").close().close();
  xml.flush();
}

void loop() {
}
```

การถอดรหัสสตริง XML:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Hello, world!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

ตัวอย่างผลลัพธ์:

```
<greeting>
  <text>Hello, world!</text>
</greeting>
```

## ลึกลงไป
XML หรือ Extensible Markup Language เป็นภาษามาร์กอัพที่กำหนดชุดของกฎสำหรับการเข้ารหัสเอกสารในรูปแบบที่สามารถอ่านได้โดยทั้งมนุษย์และเครื่องจักร มันมีมาตั้งแต่ปลายยุค 90 และถูกใช้งานอย่างกว้างขวางในหลายสาขา โดยเฉพาะที่ต้องการการแลกเปลี่ยนข้อมูลที่ไม่ขึ้นกับแพลตฟอร์ม Arduino ที่มีทรัพยากรหน่วยความจำจำกัดทำให้การทำงานกับ XML ยากกว่าบนพีซี ดังนั้น ไลบรารีที่มีน้ำหนักเบาจึงมีความสำคัญ แม้ว่า JSON จะได้รับความนิยมสำหรับการแลกเปลี่ยนข้อมูลเนื่องจากโครงสร้างที่ง่ายกว่าและมีขนาดเล็ก แต่ XML ยังคงถูกใช้งานอย่างแพร่หลาย เฉพาะเมื่อต้องจัดการกับระบบเก่าหรือแอปพลิเคชันที่ต้องการการตรวจสอบเอกสารผ่านสคีมา กุญแจสำคัญในการดำเนินการ XML ของ Arduino คือการแยกส่วน Stream ซึ่งอ่านเอกสารแบบแบ่งส่วนเพื่อรักษาการใช้หน่วยความจำให้ต่ำ

## ดูเพิ่มเติม
- [เอกสารข้อมูลไลบรารี TinyXML-2](https://leethomason.github.io/tinyxml2/)
- [ไลบรารี Arduino JSON](https://arduinojson.org/) สำหรับทางเลือกเมื่อทำงานกับข้อมูล JSON
- [W3Schools บทเรียน XML](https://www.w3schools.com/xml/) สำหรับการเรียนรู้ XML ทั่วไป
- [W3C ข้อกำหนด XML](https://www.w3.org/XML/) สำหรับมาตรฐานและคำแนะนำ XML อย่างเป็นทางการ
