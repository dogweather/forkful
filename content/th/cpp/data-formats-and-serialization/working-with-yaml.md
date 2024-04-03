---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:06.790885-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E43\u0E19\u0E01\u0E32\u0E23\
  \u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML \u0E43\u0E19 C++, \u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E22\u0E2D\u0E14\u0E19\u0E34\u0E22\u0E21\u0E04\
  \u0E37\u0E2D\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 `yaml-cpp` \u0E02\u0E31\u0E49\
  \u0E19\u0E41\u0E23\u0E01, \u0E43\u0E2B\u0E49\u0E41\u0E19\u0E48\u0E43\u0E08\u0E27\
  \u0E48\u0E32\u0E04\u0E38\u0E13\u0E44\u0E14\u0E49\u0E15\u0E34\u0E14\u0E15\u0E31\u0E49\
  \u0E07 `yaml-cpp` \u0E41\u0E25\u0E30\u0E40\u0E0A\u0E37\u0E48\u0E2D\u0E21\u0E42\u0E22\
  \u0E07\u0E01\u0E31\u0E1A\u0E42\u0E1B\u0E23\u0E40\u0E08\u0E47\u0E01\u0E15\u0E4C C++\u2026"
lastmod: '2024-03-17T21:57:56.541176-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\
  \u0E1A YAML \u0E43\u0E19 C++, \u0E15\u0E31\u0E27\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E22\
  \u0E2D\u0E14\u0E19\u0E34\u0E22\u0E21\u0E04\u0E37\u0E2D\u0E44\u0E25\u0E1A\u0E23\u0E32\
  \u0E23\u0E35 `yaml-cpp` \u0E02\u0E31\u0E49\u0E19\u0E41\u0E23\u0E01, \u0E43\u0E2B\
  \u0E49\u0E41\u0E19\u0E48\u0E43\u0E08\u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\u0E44\u0E14\
  \u0E49\u0E15\u0E34\u0E14\u0E15\u0E31\u0E49\u0E07 `yaml-cpp` \u0E41\u0E25\u0E30\u0E40\
  \u0E0A\u0E37\u0E48\u0E2D\u0E21\u0E42\u0E22\u0E07\u0E01\u0E31\u0E1A\u0E42\u0E1B\u0E23\
  \u0E40\u0E08\u0E47\u0E01\u0E15\u0E4C C++ \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E16\u0E39\u0E01\u0E15\u0E49\u0E2D\u0E07\n\n**\u0E01\u0E32\
  \u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C YAML:**."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
weight: 41
---

## วิธีทำ:
ในการทำงานกับ YAML ใน C++, ตัวเลือกยอดนิยมคือไลบรารี `yaml-cpp` ขั้นแรก, ให้แน่ใจว่าคุณได้ติดตั้ง `yaml-cpp` และเชื่อมโยงกับโปรเจ็กต์ C++ ของคุณอย่างถูกต้อง

**การอ่านไฟล์ YAML:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Title: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

กำหนดให้ `config.yaml` มีลักษณะดังนี้:

```yaml
title: "Example YAML"
```

การรันโค้ด C++ ด้านบนจะส่งผลลัพธ์:

```
Title: Example YAML
```

**การเขียนไปยังไฟล์ YAML:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "Example YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

โค้ดนี้จะสร้าง `output.yaml` ที่มีเนื้อหา:

```yaml
title: Example YAML
```

ตัวอย่างเหล่านี้เป็นการแนะนำเบื้องต้นในการอ่านและเขียนไฟล์ YAML ใน C++ โดยใช้ไลบรารี `yaml-cpp` สำหรับโครงสร้างที่ซับซ้อนมากขึ้นและกรณีการใช้งานแบบอื่นๆ สำรวจเอกสารของ `yaml-cpp` เพื่อหาคุณสมบัติเช่นลำดับ, ป้ายกำกับ, และเทคนิคการซีเรียไลซ์และการถอดรหัสที่ซับซ้อนมากขึ้น
