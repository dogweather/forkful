---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:34.285241-06:00
description: "JSON (JavaScript Object Notation) \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E40\u0E1A\u0E32\u0E41\u0E25\u0E30\u0E07\
  \u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E41\
  \u0E25\u0E30\u0E02\u0E19\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u2026"
lastmod: '2024-03-17T21:57:56.542102-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\
  \u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E40\u0E1A\u0E32\u0E41\u0E25\u0E30\u0E07\u0E48\
  \u0E32\u0E22\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E41\u0E25\
  \u0E30\u0E02\u0E19\u0E2A\u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
weight: 38
---

## จุดประสงค์และเหตุผล

JSON (JavaScript Object Notation) เป็นรูปแบบที่เบาและง่ายต่อการเก็บและขนส่งข้อมูล ทำให้เป็นสื่อที่ยอดเยี่ยมสำหรับการแลกเปลี่ยนข้อมูลระหว่างเซิร์ฟเวอร์และเว็บแอปพลิเคชัน โปรแกรมเมอร์ใช้ JSON ต่อเพราะความสามารถในการอ่านและความเรียบง่ายในการแยกวิเคราะห์โดยคอมพิวเตอร์ โดยเฉพาะเมื่อทำงานเกี่ยวกับแอปพลิเคชันที่ต้องการการแลกเปลี่ยนข้อมูลผ่านอินเทอร์เน็ตหรือการตั้งค่าคอนฟิก

## วิธีการ:

ใน C++ ไม่มีการสนับสนุน JSON โดยตรง แต่ห้องสมุดของบุคคลที่สามเช่น nlohmann/json ทำให้การใช้งานง่าย นี่คือวิธีใช้สำหรับงานพื้นฐาน:

ก่อนอื่น ตรวจสอบให้แน่ใจว่าคุณมีไลบรารีติดตั้งอยู่ หากคุณใช้ package manager อย่าง vcpkg หรือ Conan คุณสามารถเพิ่ม `nlohmann/json` ลงในโปรเจคของคุณได้อย่างง่ายดาย

### การแยกวิเคราะห์ JSON จากสตริง

```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // ข้อมูล JSON เป็นสตริง
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // แยกวิเคราะห์สตริง JSON
    auto jsonObject = nlohmann::json::parse(jsonData);

    // การเข้าถึงข้อมูล
    std::cout << "Name: " << jsonObject["name"] << "\n"
              << "Age: " << jsonObject["age"] << "\n"
              << "City: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**ผลลัพธ์ตัวอย่าง:**

```
Name: John
Age: 30
City: New York
```

### การสร้าง JSON

การสร้างข้อมูล JSON ก็เป็นเรื่องง่ายไม่แพ้กัน เพียงแค่กำหนดค่าให้กับวัตถุ `nlohmann::json`.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // การสร้างวัตถุ JSON
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // แปลงวัตถุ JSON เป็นสตริงและพิมพ์ออกมา
    std::string jsonString = jsonObject.dump(4); // อาร์กิวเมนต์ 4 สำหรับการพิมพ์สวยงาม
    std::cout << jsonString << std::endl;

    return 0;
}
```

**ผลลัพธ์ตัวอย่าง:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

ตัวอย่างเหล่านี้แสดงการใช้งานหลักสำหรับการทำงานกับ JSON ใน C++ โดยใช้ไลบรารี `nlohmann/json`. ด้วยพื้นฐานเหล่านี้ คุณสามารถแยกวิเคราะห์และสร้าง JSON สำหรับแอปพลิเคชันต่างๆ ตั้งแต่ไฟล์คอนฟิกจนถึงการแลกเปลี่ยนข้อมูลในแอปพลิเคชันเครือข่าย.
