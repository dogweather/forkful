---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:28.086111-06:00
description: "\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E1B\u0E01\u0E15\u0E34, \u0E2B\
  \u0E23\u0E37\u0E2D regex, \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E19\u0E31\
  \u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E04\u0E49\
  \u0E19\u0E2B\u0E32, \u0E08\u0E31\u0E1A\u0E04\u0E39\u0E48, \u0E41\u0E25\u0E30\u0E08\
  \u0E31\u0E14\u0E01\u0E32\u0E23\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E14\u0E49\u0E27\u0E22\
  \u0E40\u0E17\u0E04\u0E19\u0E34\u0E04\u0E01\u0E32\u0E23\u0E08\u0E31\u0E1A\u0E04\u0E39\
  \u0E48\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E31\u0E49\u0E19\u0E2A\u0E39\u0E07\
  \ \u0E43\u0E19 Rust, \u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 regex\u2026"
lastmod: '2024-03-17T21:57:55.978563-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E1B\u0E01\u0E15\u0E34, \u0E2B\u0E23\
  \u0E37\u0E2D regex, \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E19\u0E31\u0E01\
  \u0E1E\u0E31\u0E12\u0E19\u0E32\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E04\u0E49\u0E19\
  \u0E2B\u0E32, \u0E08\u0E31\u0E1A\u0E04\u0E39\u0E48, \u0E41\u0E25\u0E30\u0E08\u0E31\
  \u0E14\u0E01\u0E32\u0E23\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E14\u0E49\u0E27\u0E22\u0E40\
  \u0E17\u0E04\u0E19\u0E34\u0E04\u0E01\u0E32\u0E23\u0E08\u0E31\u0E1A\u0E04\u0E39\u0E48\
  \u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E31\u0E49\u0E19\u0E2A\u0E39\u0E07 \u0E43\
  \u0E19 Rust, \u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 regex\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E23\u0E01\u0E38\u0E25\u0E32\u0E23\
  \u0E4C\u0E40\u0E2D\u0E47\u0E01\u0E40\u0E1E\u0E23\u0E2A\u0E0A\u0E31\u0E19"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

นิพจน์ปกติ, หรือ regex, ช่วยให้นักพัฒนาสามารถค้นหา, จับคู่, และจัดการสตริงด้วยเทคนิคการจับคู่รูปแบบขั้นสูง ใน Rust, การใช้งาน regex ช่วยในการแยกวิเคราะห์และจัดการข้อมูลข้อความได้อย่างมีประสิทธิภาพ ทำให้งานเช่นการตรวจสอบความถูกต้องของข้อมูล, การค้นหา, และการเปลี่ยนแปลงข้อความเป็นไปได้อย่างง่ายดายและสามารถบำรุงรักษาได้ 

## วิธีการ:

ไลบรารี `regex` ของ Rust เป็นตัวเลือกหลักในการทำงานกับนิพจน์ปกติ ในการใช้งาน, คุณจะต้องเพิ่มมันเข้าไปในไฟล์ `Cargo.toml` ของคุณก่อน:

```toml
[dependencies]
regex = "1"
```

จากนั้น, คุณสามารถเริ่มต้นทำงานกับฟังก์ชันของ regex ในโค้ด Rust ของคุณ นี่คือวิธีการทำงานบางอย่างที่พบบ่อย:

### การจับคู่รูปแบบในสตริง

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-15";

    println!("ข้อความนี้ตรงกับรูปแบบวันที่หรือไม่? {}", re.is_match(date));
    // ผลลัพธ์: ข้อความนี้ตรงกับรูปแบบวันที่หรือไม่? true
}
```

### การค้นหาและเข้าถึงการจับคู่

```rust
use regex::Regex;

fn main() {
    let text = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(text) {
        println!("ภาษา: {}, ปี: {}", &cap[1], &cap[2]);
    }
    // ผลลัพธ์:
    // ภาษา: Rust, ปี: 2023
    // ภาษา: C++, ปี: 2022
    // ภาษา: Python, ปี: 2021
}
```

### การแทนที่ข้อความ

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let text = "Rust 2023, C++ 2022, Python 2021";
    let replaced = re.replace_all(text, "$1 ได้อัพเดทในปี $2");

    println!("ข้อความที่อัพเดท: {}", replaced);
    // ผลลัพธ์: ข้อความที่อัพเดท: Rust ได้อัพเดทในปี 2023, C++ ได้อัพเดทในปี 2022, Python ได้อัพเดทในปี 2021
}
```

### การแบ่งข้อความโดยใช้ Regex

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // แบ่งที่อักขระที่ไม่ใช่คำ
    let text = "Rust-C++-Python-Go";

    let fields: Vec<&str> = re.split(text).collect();

    for field in fields {
        println!("ภาษา: {}", field);
    }
    // ผลลัพธ์:
    // ภาษา: Rust
    // ภาษา: C++
    // ภาษา: Python
    // ภาษา: Go
}
```

ตัวอย่างเหล่านี้ให้คำแนะนำพื้นฐานเกี่ยวกับการเริ่มต้นใช้งานนิพจน์ปกติใน Rust ในขณะที่ความต้องการของคุณกลายเป็นสิ่งที่ซับซ้อนมากขึ้น, คราต `regex` มีฟังก์ชันการทำงานมากมายสำหรับงานจับคู่รูปแบบที่ซับซ้อนและการจัดการข้อความ.
