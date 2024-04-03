---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:02.672615-06:00
description: "TOML (Tom's Obvious, Minimal Language) \u0E40\u0E1B\u0E47\u0E19\u0E23\
  \u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\
  \u0E25\u0E44\u0E25\u0E0B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\
  \u0E2D\u0E48\u0E32\u0E19\u0E07\u0E48\u0E32\u0E22\u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\
  \u0E08\u0E32\u0E01\u0E21\u0E35\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E17\
  \u0E35\u0E48\u0E0A\u0E31\u0E14\u0E40\u0E08\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49 TOML\u2026"
lastmod: '2024-03-17T21:57:56.543961-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E25\
  \u0E44\u0E25\u0E0B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\
  \u0E48\u0E32\u0E19\u0E07\u0E48\u0E32\u0E22\u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\
  \u0E32\u0E01\u0E21\u0E35\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E17\u0E35\
  \u0E48\u0E0A\u0E31\u0E14\u0E40\u0E08\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49 TOML \u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\
  \u0E48\u0E32\u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01\u0E21\u0E31\u0E19\
  \u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E21\u0E14\u0E38\u0E25\u0E23\u0E30\u0E2B\u0E27\
  \u0E48\u0E32\u0E07\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E02\u0E2D\u0E07\u0E21\u0E19\u0E38\
  \u0E29\u0E22\u0E4C\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\
  \u0E32\u0E30\u0E2B\u0E4C\u0E02\u0E2D\u0E07\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\
  ."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## วิธีการ:
ในการทำงานกับ TOML ใน C++ คุณจะต้องมีไลบรารีเช่น `toml++` นี่คือการเริ่มต้นอย่างรวดเร็ว:

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // วิเคราะห์ TOML จากไฟล์
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // การเข้าถึงค่า
    std::string title = config["title"].value_or("Untitled");
    std::cout << "Title: " << title << '\n';

    // แก้ไขและบันทึก TOML
    config["title"] = "New Title";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

ตัวอย่าง `config.toml`:
```toml
title = "Example"
```

ตัวอย่างผลลัพธ์:
```plaintext
Title: Example
```

## ดำดิ่งลึก
TOML ถูกสร้างขึ้นโดย Tom Preston-Werner ในปี 2013 เป็นทางเลือกสำหรับ YAML และ JSON มันถูกออกแบบมาเพื่อความเรียบง่ายและชัดเจน โดยหลักสำหรับไฟล์การกำหนดค่า ไม่เหมือนกับ JSON, TOML มุ่งเน้นไปที่การไม่กำกวมซึ่งหมายความว่ามันเป็นอย่างชัดเจนในวิธีการวิเคราะห์เอกสาร

ทางเลือกอื่นๆ สำหรับ TOML มี YAML ซึ่งมีความยืดหยุ่นมากขึ้นในสิ่งที่อนุญาต แม้บางครั้งอาจทำให้คาดเดาไม่ได้ JSON, อีกทางเลือกหนึ่ง, เป็นโครงสร้างที่เข้มงวด แต่ไม่เป็นมิตรกับมนุษย์สำหรับการกำหนดค่าเนื่องจากไม่มีความคิดเห็นและการใช้วงเล็บมากเกินไป

ในการใช้งาน `toml++` เป็นไลบรารี C++17 ที่เป็นเพียงไฟล์ header เท่านั้น และตรงตามข้อกำหนด TOML ล่าสุด มันให้การเข้าถึงแบบ DOM-like เพื่อนำทางและจัดการข้อมูล TOML ทำให้การรวมเข้ากับโปรเจกต์เป็นเรื่องง่าย ไลบรารีจัดการกับการวิเคราะห์ การตรวจสอบความถูกต้อง และการสร้างผลลัพธ์ ช่วยให้คุณสามารถรับและกำหนดข้อมูล TOML โดยใช้ประเภทข้อมูล C++

## ดูเพิ่มเติม
- ที่เก็บข้อมูลบน GitHub ของ TOML: https://github.com/toml-lang/toml
- `toml++`, ไลบรารี C++ สำหรับ TOML: https://github.com/marzer/tomlplusplus
- เอกสารอย่างเป็นทางการของ TOML ที่มีการอธิบายรูปแบบอย่างละเอียด: https://toml.io/en/
