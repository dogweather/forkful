---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:07.990774-06:00
description: "\u0E41\u0E2D\u0E2A\u0E42\u0E0B\u0E0B\u0E34\u0E40\u0E2D\u0E17\u0E35\u0E1F\
  \u0E2D\u0E32\u0E23\u0E4C\u0E40\u0E23\u0E22\u0E4C \u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E35\
  \u0E48\u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E0B\u0E2D\u0E1F\u0E15\u0E4C\
  \u0E41\u0E27\u0E23\u0E4C\u0E43\u0E19\u0E0A\u0E38\u0E21\u0E0A\u0E19 Rust \u0E40\u0E23\
  \u0E35\u0E22\u0E01\u0E27\u0E48\u0E32 \"hash maps\" \u0E40\u0E1B\u0E47\u0E19\u0E04\
  \u0E2D\u0E25\u0E40\u0E25\u0E01\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\
  \u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E04\u0E39\u0E48\u0E02\u0E2D\u0E07\u0E04\u0E35\u0E22\u0E4C\u0E41\u0E25\u0E30\
  \u0E04\u0E48\u0E32\u2026"
lastmod: '2024-03-17T21:57:55.982544-06:00'
model: gpt-4-0125-preview
summary: "\u0E41\u0E2D\u0E2A\u0E42\u0E0B\u0E0B\u0E34\u0E40\u0E2D\u0E17\u0E35\u0E1F\
  \u0E2D\u0E32\u0E23\u0E4C\u0E40\u0E23\u0E22\u0E4C \u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E35\
  \u0E48\u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E0B\u0E2D\u0E1F\u0E15\u0E4C\
  \u0E41\u0E27\u0E23\u0E4C\u0E43\u0E19\u0E0A\u0E38\u0E21\u0E0A\u0E19 Rust \u0E40\u0E23\
  \u0E35\u0E22\u0E01\u0E27\u0E48\u0E32 \"hash maps\" \u0E40\u0E1B\u0E47\u0E19\u0E04\
  \u0E2D\u0E25\u0E40\u0E25\u0E01\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\
  \u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E04\u0E39\u0E48\u0E02\u0E2D\u0E07\u0E04\u0E35\u0E22\u0E4C\u0E41\u0E25\u0E30\
  \u0E04\u0E48\u0E32\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E41\u0E2D\u0E40\u0E23\u0E22\u0E4C\u0E2A\
  \u0E21\u0E32\u0E0A\u0E34\u0E01"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

แอสโซซิเอทีฟอาร์เรย์ หรือที่นักพัฒนาซอฟต์แวร์ในชุมชน Rust เรียกว่า "hash maps" เป็นคอลเลกชันที่เก็บข้อมูลในรูปแบบคู่ของคีย์และค่า โปรแกรมเมอร์ใช้พวกมันสำหรับการค้นหาข้อมูลอย่างรวดเร็ว ทำให้สามารถจัดการข้อมูลได้อย่างมีประสิทธิภาพบนคีย์ที่ไม่ซ้ำกัน

## วิธีการ:

ใน Rust, ประเภท `HashMap` จากโมดูล `std::collections` มอบความสามารถของแอสโซซิเอทีฟอาร์เรย์ นี่คือวิธีที่คุณสามารถทำงานกับพวกมันได้:

```Rust
use std::collections::HashMap;

fn main() {
    // การสร้าง HashMap ใหม่
    let mut scores = HashMap::new();

    // การแทรกค่า
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // การเข้าถึงค่า
    let team_name = String::from("Blue");
    if let Some(score) = scores.get(&team_name) {
        println!("คะแนนของทีม Blue: {}", score); // ผลลัพธ์: คะแนนของทีม Blue: 10
    }

    // การอัปเดตค่า
    scores.entry(String::from("Blue")).and_modify(|e| *e += 5);

    // การวนลูปผ่านคู่ของคีย์-ค่า
    for (key, value) in &scores {
        println!("{}: {}", key, value); // ผลลัพธ์: Blue: 15, Yellow: 50
    }
}
```

## ศึกษาลึกลงไป

`HashMap` ใน Rust ใช้ฟังก์ชันการแฮชเพื่อแม็ปคีย์ไปยังค่า ซึ่งช่วยให้สามารถดึงข้อมูลได้อย่างรวดเร็ว อย่างไรก็ตาม ความสามารถนี้มาพร้อมกับต้นทุน: hash maps ไม่รักษาลำดับขององค์ประกอบ นั่นเป็นการต่างจากการใช้งานแอสโซซิเอทีฟอาร์เรย์ในภาษาอื่นๆ เช่น Python (`dict`) หรือ Ruby ซึ่งในเวอร์ชันล่าสุดรักษาลำดับของการแทรกเป็นคุณสมบัติพิเศษ สำหรับกรณีที่ลำดับของคู่คีย์-ค่ามีความสำคัญ นักพัฒนาใน Rust อาจพิจารณาใช้ `BTreeMap` จากโมดูล `std::collections` ซึ่งรักษาลำดับได้ แต่อาจมีความช้าในการแทรกและดึงข้อมูลเมื่อเทียบกับ `HashMap` ในท้ายที่สุด การเลือกใช้ระหว่าง `HashMap` กับ `BTreeMap` ขึ้นอยู่กับความต้องการเฉพาะด้านลำดับและประสิทธิภาพ
