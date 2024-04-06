---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:55.717938-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: C++ \u0E40\u0E2A\u0E19\u0E2D\u0E27\
  \u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E2B\u0E25\u0E32\u0E22\u0E27\u0E34\u0E18\u0E35\
  \u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\
  \u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 \u0E41\u0E15\u0E48\
  \u0E2B\u0E19\u0E36\u0E48\u0E07\u0E43\u0E19\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\
  \u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\u0E17\u0E35\u0E48\u0E2A\
  \u0E38\u0E14\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\
  \u0E23\u0E32\u0E23\u0E35 `<fstream>` \u0E0B\u0E36\u0E48\u0E07\u0E21\u0E35\u0E04\u0E25\
  \u0E32\u0E2A `ofstream` (output file\u2026"
lastmod: '2024-03-17T21:57:56.539343-06:00'
model: gpt-4-0125-preview
summary: "C++ \u0E40\u0E2A\u0E19\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E2B\
  \u0E25\u0E32\u0E22\u0E27\u0E34\u0E18\u0E35\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\
  \u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25 \u0E41\u0E15\u0E48\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E43\u0E19\
  \u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\
  \u0E07\u0E21\u0E32\u0E17\u0E35\u0E48\u0E2A\u0E38\u0E14\u0E04\u0E37\u0E2D\u0E01\u0E32\
  \u0E23\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 `<fstream>` \u0E0B\
  \u0E36\u0E48\u0E07\u0E21\u0E35\u0E04\u0E25\u0E32\u0E2A `ofstream` (output file stream)\
  \ \u0E17\u0E35\u0E48\u0E2D\u0E2D\u0E01\u0E41\u0E1A\u0E1A\u0E21\u0E32\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\
  \u0E25\u0E4C\n"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 24
---

## วิธีทำ:
C++ เสนอวิธีการหลายวิธีในการเขียนไปยังไฟล์ข้อมูล แต่หนึ่งในวิธีที่ตรงไปตรงมาที่สุดคือการใช้ไลบรารี `<fstream>` ซึ่งมีคลาส `ofstream` (output file stream) ที่ออกแบบมาสำหรับงานเขียนไฟล์

### ตัวอย่างการใช้ `<fstream>`:
```cpp
#include <fstream>
#include <iostream>

int main() {
    std::ofstream file("example.txt");
    if (file.is_open()) {
        file << "Hello, world!\n";
        file << "การเขียนไฟล์ใน C++ เป็นเรื่องง่าย.";
        file.close();
    } else {
        std::cerr << "ไม่สามารถเปิดไฟล์ได้\n";
    }
    return 0;
}
```

**ตัวอย่างผลลัพธ์ใน 'example.txt':**
```
Hello, world!
การเขียนไฟล์ใน C++ เป็นเรื่องง่าย.
```

เมื่อต้องการจัดการกับข้อมูลที่ซับซ้อนมากขึ้นหรือต้องการควบคุมกระบวนการเขียนมากขึ้น โปรแกรมเมอร์อาจหันไปใช้ไลบรารีของบุคคลที่สาม เช่น Boost Filesystem

### ตัวอย่างการใช้ Boost Filesystem:
เพื่อใช้งาน Boost สำหรับการจัดการไฟล์ คุณจะต้องติดตั้งไลบรารี Boost ก่อน ตัวอย่างต่อไปนี้แสดงการสร้างและเขียนลงไปในไฟล์โดยใช้ `boost::filesystem` และ `boost::iostreams`

```cpp
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/stream.hpp>
#include <iostream>

namespace io = boost::iostreams;
namespace fs = boost::filesystem;

int main() {
    fs::path filePath("boost_example.txt");
    io::stream_buffer<io::file_sink> buf(filePath.string());
    std::ostream out(&buf);
    out << "Boost ทำให้การจัดการไฟล์เป็นเรื่องง่าย.\n";
    out << "นี่คือบรรทัดที่เขียนด้วย Boost.";
    
    return 0;
}
```

**ตัวอย่างผลลัพธ์ใน 'boost_example.txt':**
```
Boost ทำให้การจัดการไฟล์เป็นเรื่องง่าย.
นี่คือบรรทัดที่เขียนด้วย Boost.
```

การเลือกใช้งานระหว่าง C++ แบบดิบและไลบรารีของบุคคลที่สามเช่น Boost อาจขึ้นอยู่กับความต้องการเฉพาะของโปรเจ็กต์ของคุณและมากน้อยเพียงใดที่คุณต้องการควบคุมหรือความยืดหยุ่นในการดำเนินการเกี่ยวกับไฟล์ I/O
