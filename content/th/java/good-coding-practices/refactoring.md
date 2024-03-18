---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:23.687544-06:00
description: "\u0E01\u0E32\u0E23\u0E23\u0E35\u0E41\u0E1F\u0E04\u0E40\u0E15\u0E2D\u0E23\
  \u0E4C\u0E04\u0E37\u0E2D\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E02\
  \u0E2D\u0E07\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\
  \u0E23\u0E49\u0E32\u0E07\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E04\u0E2D\u0E21\
  \u0E1E\u0E34\u0E27\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\
  \u0E22\u0E39\u0E48 \u2014 \u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\
  \u0E25\u0E07\u0E01\u0E32\u0E23\u0E41\u0E1F\u0E04\u0E40\u0E15\u0E2D\u0E23\u0E4C \u2014\
  \ \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\
  \u0E41\u0E1B\u0E25\u0E07\u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E20\u0E32\
  \u0E22\u0E19\u0E2D\u0E01\u0E02\u0E2D\u0E07\u0E21\u0E31\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.089952-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E23\u0E35\u0E41\u0E1F\u0E04\u0E40\u0E15\u0E2D\u0E23\
  \u0E4C\u0E04\u0E37\u0E2D\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E02\
  \u0E2D\u0E07\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\
  \u0E23\u0E49\u0E32\u0E07\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E04\u0E2D\u0E21\
  \u0E1E\u0E34\u0E27\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\
  \u0E22\u0E39\u0E48 \u2014 \u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\
  \u0E25\u0E07\u0E01\u0E32\u0E23\u0E41\u0E1F\u0E04\u0E40\u0E15\u0E2D\u0E23\u0E4C \u2014\
  \ \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\
  \u0E41\u0E1B\u0E25\u0E07\u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E20\u0E32\
  \u0E22\u0E19\u0E2D\u0E01\u0E02\u0E2D\u0E07\u0E21\u0E31\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
---

{{< edit_this_page >}}

## คืออะไร & ทำไมถึงต้องทำ?
การรีแฟคเตอร์คือกระบวนการของการปรับโครงสร้างของโค้ดคอมพิวเตอร์ที่มีอยู่ — เปลี่ยนแปลงการแฟคเตอร์ — โดยไม่เปลี่ยนแปลงพฤติกรรมภายนอกของมัน โปรแกรมเมอร์ทำเช่นนี้เพื่อปรับปรุงคุณลักษณะที่ไม่ใช่ฟังก์ชันของซอฟต์แวร์ เช่น เพิ่มความสามารถในการอ่าน ลดความซับซ้อน และทำให้โค้ดง่ายต่อการบำรุงรักษาสำหรับโครงการในอนาคต

## วิธีทำ:
ลองดูคลาส Java แบบง่ายๆ ที่ต้องการการรีแฟคเตอร์เนื่องจากมีการจัดการและความชัดเจนที่ไม่ดี

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // การดำเนินการอื่นๆ...
    }
}
```

หลังจากรีแฟคเตอร์ เรามี:

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // การดำเนินการอื่นๆ...
}
```

โดยการรีแฟคเตอร์ เราได้ปรับปรุงชื่อและพารามิเตอร์ของเมธอดเพื่อความสามารถในการอ่าน และลบการใช้งานสาขาเงื่อนไขภายในเมธอดเดียว การดำเนินการแต่ละอย่างตอนนี้ระบุวัตถุประสงค์ของมันอย่างชัดเจน

## ลึกเข้าไป:
การรีแฟคเตอร์มีต้นกำเนิดในชุมชน Smalltalk ด้วยความสนใจในความสามารถในการอ่านโค้ดและการออกแบบที่เน้นวัตถุ (object-oriented design) แต่มันจริงๆ เริ่มติดตลาดในโลกของ Java ในช่วงปลายปี 90 และต้นปี 00 โดยเฉพาะหลังจากการเผยแพร่หนังสือสำคัญของ Martin Fowler ที่ชื่อว่า "Refactoring: Improving the Design of Existing Code."

มีทางเลือกอื่นนอกจากการรีแฟคเตอร์ เช่น เขียนโค้ดจากเริ่มต้นใหม่ อย่างไรก็ตาม การรีแฟคเตอร์มักจะได้รับความนิยมเพราะว่ามันเกี่ยวข้องกับการเปลี่ยนแปลงทีละน้อยที่ไม่ทำให้ฟังก์ชันก์ชันของแอปพลิเคชันต้องหยุดชะงัก

รายละเอียดการดำเนินการเมื่อรีแฟคเตอร์ใน Java (หรือภาษาโปรแกรมอื่นๆ) ต้องการความเข้าใจเกี่ยวกับกลิ่นโค้ด — ตัวบ่งชี้ของปัญหาลึกๆ ในโค้ด กลิ่นบางอย่างรวมถึงเมธอดยาวๆ, คลาสใหญ่, โค้ดที่ซ้ำกัน และการใช้งานรูปแบบพื้นฐานมากเกินไป โดยการใช้รูปแบบการรีแฟคเตอร์ เช่น Extract Method, Move Method, หรือ Replace Temp with Query, นักพัฒนาสามารถจัดการกับกลิ่นเหล่านี้ได้อย่างเป็นระบบในขณะที่รักษาโค้ดให้ทำงานได้ตลอดเวลา

เครื่องมืออัตโนมัติ เช่น การรองรับการรีแฟคเตอร์ของ IntelliJ IDEA หรือปลั๊กอินสำหรับ Eclipse สามารถช่วยในกระบวนการด้วยการทำ automation การรีแฟคเตอร์ เช่น การเปลี่ยนชื่อตัวแปร เมธอด และคลาส การแยกเมธอดหรือตัวแปรออกมา และการย้ายเมธอดหรือคลาสไปยังแพ็คเกจหรือเนมสเปซที่แตกต่างกัน

## ดูเพิ่มเติม:
- หนังสือของ Martin Fowler "Refactoring: Improving the Design of Existing Code": https://martinfowler.com/books/refactoring.html
- เทคนิคการรีแฟคเตอร์บน Refactoring.Guru: https://refactoring.guru/refactoring/techniques
- การรีแฟคเตอร์อัตโนมัติใน Eclipse: https://www.eclipse.org/eclipse/news/4.18/jdt.php
- คุณสมบัติการรีแฟคเตอร์ของ IntelliJ IDEA: https://www.jetbrains.com/idea/features/refactoring.html

แต่ละทรัพยากรนี้ให้พื้นฐานในการเข้าใจหลักการของการรีแฟคเตอร์หรือเครื่องมือที่สามารถใช้ในการนำหลักการเหล่านี้ไปใช้ได้
