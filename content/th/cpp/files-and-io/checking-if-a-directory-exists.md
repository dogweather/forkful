---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:45.300439-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 C++ \u0E23\u0E38\
  \u0E48\u0E19\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19 (C++17 \u0E41\u0E25\
  \u0E30\u0E2B\u0E25\u0E31\u0E07\u0E08\u0E32\u0E01\u0E19\u0E31\u0E49\u0E19) \u0E04\
  \u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\
  \u0E23\u0E32\u0E23\u0E35\u0E23\u0E30\u0E1A\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\
  \u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\u0E39\u0E48\
  \u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48\u2026"
lastmod: '2024-03-17T21:57:56.535430-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 C++ \u0E23\u0E38\u0E48\u0E19\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\
  \u0E31\u0E19 (C++17 \u0E41\u0E25\u0E30\u0E2B\u0E25\u0E31\u0E07\u0E08\u0E32\u0E01\
  \u0E19\u0E31\u0E49\u0E19) \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E23\u0E30\u0E1A\u0E1A\
  \u0E44\u0E1F\u0E25\u0E4C\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\
  \u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\
  \u0E23\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48 \u0E21\
  \u0E31\u0E19\u0E43\u0E2B\u0E49\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E17\u0E35\
  \u0E48\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\u0E41\u0E25\u0E30\
  \u0E21\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E43\u0E19\u0E01\u0E32\u0E23\
  \u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E23\u0E30\
  \u0E1A\u0E1A\u0E44\u0E1F\u0E25\u0E4C \u0E23\u0E27\u0E21\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E2B\u0E23\
  \u0E37\u0E2D\u0E44\u0E21\u0E48."
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
weight: 20
---

## วิธีการ:
ใน C++ รุ่นปัจจุบัน (C++17 และหลังจากนั้น) คุณสามารถใช้ไลบรารีระบบไฟล์เพื่อตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่ มันให้วิธีการที่ตรงไปตรงมาและมีมาตรฐานในการดำเนินการกับระบบไฟล์ รวมถึงการตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "The directory exists." << std::endl;
    } else {
        std::cout << "The directory does not exist." << std::endl;
    }

    return 0;
}
```
ตัวอย่างผลลัพธ์หากมีไดเรกทอรีอยู่:
```
The directory exists.
```

ตัวอย่างผลลัพธ์หากไม่มีไดเรกทอรีอยู่:
```
The directory does not exist.
```

สำหรับโปรเจ็กต์ที่ยังไม่ใช้ C++17 หรือสำหรับคุณสมบัติเพิ่มเติม ไลบรารี Boost Filesystem เป็นตัวเลือกยอดนิยมจากบุคคลที่สามที่มีฟังก์ชันคล้ายคลึงกัน

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "The directory exists." << std::endl;
    } else {
        std::cout << "The directory does not exist." << std::endl;
    }

    return 0;
}
```
หากใช้ Boost Filesystem ผลลัพธ์จะเหมือนกับตัวอย่างของระบบไฟล์ C++17 ขึ้นอยู่กับว่ามีไดเรกทอรีอยู่ที่เส้นทางที่ระบุหรือไม่
