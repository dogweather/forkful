---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:15.764844-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: ."
lastmod: '2024-03-17T21:57:56.542972-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## วิธีการ:


### การอ่านไฟล์ CSV โดยใช้ห้องสมุดมาตรฐานของ C++:
```cpp
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

int main() {
    std::ifstream file("data.csv");
    std::string line;
    
    while (std::getline(file, line)) {
        std::stringstream lineStream(line);
        std::string cell;
        std::vector<std::string> parsedRow;
        
        while (std::getline(lineStream, cell, ',')) {
            parsedRow.push_back(cell);
        }
        
        // ทำการประมวลผล parsedRow ที่นี่
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### การเขียนลงไฟล์ CSV:
```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> data = {
        {"Name", "Age", "City"},
        {"John Doe", "29", "New York"},
        {"Jane Smith", "34", "Los Angeles"}
    };
    
    for (const auto& row : data) {
        for (size_t i = 0; i < row.size(); i++) {
            file << row[i];
            if (i < row.size() - 1) file << ",";
        }
        file << "\n";
    }
    
    return 0;
}
```

### การใช้งานไลบรารีของบุคคลที่สาม: `csv2`:
ในขณะที่ห้องสมุดมาตรฐานของ C++ ให้เครื่องมือพื้นฐานสำหรับการทำงานกับไฟล์และสตริง การใช้ประโยชน์จากไลบรารีของบุคคลที่สามสามารถทำให้การประมวลผล CSV เรียบง่ายยิ่งขึ้น ไลบรารีหนึ่งที่ทำให้การทำงานนี้ง่ายขึ้นคือ `csv2` ที่เป็นที่รู้จักกันด้วยความง่ายในการใช้งานและประสิทธิภาพ

- การติดตั้ง: โดยปกติจะติดตั้งผ่านตัวจัดการแพ็กเกจเช่น Conan หรือตรงจากโรงเก็บ GitHub ของมัน

ตัวอย่างการใช้ `csv2` เพื่ออ่านไฟล์ CSV:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto row : csv) {
            for (const auto cell : row) {
                std::cout << cell.second << "\t"; // พิมพ์ค่าของแต่ละเซลล์
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

ผลลัพธ์ตัวอย่างสำหรับการดำเนินการอ่านอาจมีลักษณะดังนี้ (สมมติว่าเป็นไฟล์ CSV สามคอลัมน์ง่ายๆ):

```
John    29    New York    
Jane    34    Los Angeles
```

ตัวอย่างเหล่านี้มีจุดมุ่งหมายเพื่อครอบคลุมการดำเนินการ CSV พื้นฐานใน C++. สำหรับสถานการณ์ที่ซับซ้อนยิ่งขึ้น เช่น การจัดการกับไฟล์ขนาดใหญ่หรือการเปลี่ยนแปลงข้อมูลที่ซับซ้อน การสำรวจไลบรารีหรือเครื่องมือที่เชี่ยวชาญอาจมีความจำเป็น
