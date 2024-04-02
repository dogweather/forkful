---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:59.143399-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C, \u0E2A\u0E23\u0E49\u0E32\
  \u0E07, \u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25 XML (eXtensible Markup Language) \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23 XML\
  \ \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E21\u0E35\u0E42\u0E04\u0E23\u0E07\u0E2A\
  \u0E23\u0E49\u0E32\u0E07,\u2026"
lastmod: '2024-03-17T21:57:56.544844-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\
  \u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C, \u0E2A\u0E23\u0E49\u0E32\u0E07, \u0E41\
  \u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \ XML (eXtensible Markup Language) \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\
  \u0E21\u0E2D\u0E23\u0E4C\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23 XML \u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E17\u0E35\u0E48\u0E21\u0E35\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\
  ,\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## อะไรและทำไม?
การทำงานกับ XML หมายถึงการแยกวิเคราะห์, สร้าง, และจัดการข้อมูล XML (eXtensible Markup Language) โปรแกรมเมอร์จัดการ XML เพื่อจัดการข้อมูลที่มีโครงสร้าง, การกำหนดค่า, และอีกมากมายเนื่องจากมีลักษณะที่เป็นกลางต่อแพลตฟอร์ม

## วิธีการ:
นี่คือวิธีง่ายๆในการแยกวิเคราะห์ XML โดยใช้ไลบรารี TinyXML-2:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Hello, World!</message></root>");
    const char* content = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << content << std::endl;
    return 0;
}
```

ผลลัพธ์ตัวอย่าง:

```
Hello, World!
```

และนี่คือวิธีการสร้างไฟล์ XML:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* declaration = doc.NewDeclaration();
    doc.InsertFirstChild(declaration);
    auto* root = doc.NewElement("root");
    doc.InsertEndChild(root);
    auto* message = doc.NewElement("message");
    message->SetText("Hello, World!");
    root->InsertEndChild(message);
    doc.SaveFile("output.xml");
    return 0;
}
```

ซึ่งจะสร้างไฟล์ XML `output.xml` ที่มีเนื้อหา:

```xml
<?xml version="1.0"?>
<root>
    <message>Hello, World!</message>
</root>
```

## ลงลึกลงไป
XML มีบทบาทสำคัญในบริการเว็บและการเก็บข้อมูลตั้งแต่ปลายยุค 90's ในขณะที่ JSON และ YAML ตอนนี้มีการใช้งานมากขึ้นสำหรับการกำหนดค่าและการทำงานร่วมกัน, XML ยังคงเป็นที่นิยมอย่างมากในระบบองค์กรมากมาย การแยกวิเคราะห์ XML ใน C++ อาจดูเหมือนเก่าด้วยการประมวลผล DOM/SAX ด้วยตนเอง โชคดีที่ไลบรารีเช่น TinyXML-2 ทำให้มันง่ายขึ้น C++ ไม่มีการสนับสนุน XML ในตัว; ไลบรารีเช่น TinyXML-2, pugixml หรือ Xerces ช่วยแก้ปัญหาที่ยากๆ

## ดูเพิ่มเติม
- เอกสาร TinyXML-2: https://leethomason.github.io/tinyxml2/
- ไลบรารี pugixml: https://pugixml.org/
- ประมวลผล Xerces-C++: https://xerces.apache.org/xerces-c/
- ข้อกำหนด XML ของ W3C: https://www.w3.org/XML/
