---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:20.952317-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E2B\u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\u0E19\u0E31\
  \u0E49\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E23\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34, \u0E14\u0E36\u0E07\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:55.991233-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E2B\u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\u0E19\u0E31\
  \u0E49\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E23\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34, \u0E14\u0E36\u0E07\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การดาวน์โหลดหน้าเว็บหมายถึงการเรียกข้อมูลที่มีอยู่ในนั้น โปรแกรมเมอร์ทำเช่นนี้เพื่อรับข้อมูล, ทดสอบอัตโนมัติ, ดึงข้อมูล, หรือตรวจสอบว่าเว็บไซต์สามารถใช้งานได้หรือไม่

## วิธีการ:

ลองดาวน์โหลดหน้าเว็บโดยใช้ `reqwest` crate ของ Rust, ซึ่งเสนอ API แบบอะซิงโครนัสที่ง่ายต่อการทำ HTTP request

ก่อนอื่น, เพิ่ม `reqwest` และ `tokio` ลงในไฟล์ `Cargo.toml` ของคุณ:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

ตอนนี้, ในโค้ด Rust ของคุณ:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let url = "http://example.com";
    let res = reqwest::get(url).await?;

    let body = res.text().await?;
    println!("Body:\n{}", body);

    Ok(())
}
```

ตัวอย่างผลลัพธ์อาจดูเป็นแบบนี้, แต่เนื้อหาจริงอาจแตกต่างกันไป:

```
Body:
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</body>
</html>
```

## รายละเอียดเพิ่มเติม

`reqwest` crate เป็นหนึ่งในวิธีที่ง่ายที่สุดในการดาวน์โหลดเนื้อหาเว็บใน Rust มันถูกพัฒนาขึ้นมาจากไลบรารี HTTP รุ่นก่อนๆ, โดยให้ทั้งส่วนติดต่อแบบซิงโครนัสและแบบอะซิงโครนัส

ทางเลือกอื่นรวมถึงไลบรารีที่มีระดับต่ำกว่าเช่น `hyper` (ซึ่ง `reqwest` เองก็ใช้ใต้ฮูด), หรือใช้การผูก `curl` สำหรับ Rust

ขั้นตอนหลักในการดาวน์โหลดหน้าเว็บประกอบด้วยการทำ HTTP GET request และการประมวลผลคำตอบ การเขียนโปรแกรมแบบอะซิงโครนัสด้วย `tokio` หมายความว่าแอปของคุณยังคงตอบสนองขณะที่การดำเนินการของเครือข่ายเสร็จสิ้น

## ดูเพิ่มเติม:

- [เอกสาร `reqwest`](https://docs.rs/reqwest/)
- [เอกสาร `tokio`](https://docs.rs/tokio/)
- [หนังสือ Rust `async`/`await`](https://rust-lang.github.io/async-book/)
- [เอกสารเว็บ MDN เกี่ยวกับ HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
