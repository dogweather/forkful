---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:54.403785-06:00
description: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious,\
  \ Minimal Language \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E01\
  \u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E44\u0E25\u0E0B\u0E4C\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E0A\u0E2D\u0E1A\u0E21\u0E31\u0E19\u0E40\u0E1E\u0E23\u0E32\u0E30\u0E04\
  \u0E27\u0E32\u0E21\u0E40\u0E23\u0E35\u0E22\u0E1A\u0E07\u0E48\u0E32\u0E22\u0E41\u0E25\
  \u0E30\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E19\u0E01\
  \u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E17\u0E35\u0E48\u0E14\u0E35;\u2026"
lastmod: '2024-03-17T21:57:56.420647-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious, Minimal\
  \ Language \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E01\u0E32\
  \u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E44\u0E25\u0E0B\u0E4C\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\
  \u0E4C\u0E0A\u0E2D\u0E1A\u0E21\u0E31\u0E19\u0E40\u0E1E\u0E23\u0E32\u0E30\u0E04\u0E27\
  \u0E32\u0E21\u0E40\u0E23\u0E35\u0E22\u0E1A\u0E07\u0E48\u0E32\u0E22\u0E41\u0E25\u0E30\
  \u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E19\u0E01\u0E32\
  \u0E23\u0E2D\u0E48\u0E32\u0E19\u0E17\u0E35\u0E48\u0E14\u0E35;\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## อะไร & ทำไม?
TOML ย่อมาจาก Tom's Obvious, Minimal Language เป็นรูปแบบการซีเรียไลซ์ข้อมูล โปรแกรมเมอร์ชอบมันเพราะความเรียบง่ายและความสามารถในการอ่านที่ดี; มันเหมาะอย่างยิ่งสำหรับไฟล์คอนฟิก มีบรรยากาศคล้ายกับ YAML แต่ไม่ยุ่งยากเหมือน JSON สำหรับมนุษย์

## วิธีการ:
เริ่มแรก ติดตั้ง `toml-cli` เพื่อเล่นกับ TOML ใน Bash มีประโยชน์สำหรับการอ่านหรือแก้ไขไฟล์ TOML ได้อย่างรวดเร็ว

```Bash
# ติดตั้ง toml-cli, เครื่องมือน้อยๆ ของเราสำหรับงาน TOML
pip install toml-cli

# ลองนึกภาพว่าคุณมีไฟล์ TOML, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# อ่านค่า
toml get config.toml owner.name
# ผลลัพธ์: Tom

# ตั้งค่า
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# ทิปเด็ด: ใช้คำพูดสำหรับคีย์ที่มีจุดหรืออักขระพิเศษ!
```

## ลงลึก
เกิดจากความไม่ชอบกับอุปสรรคของ JSON สำหรับมนุษย์, TOML เกิดขึ้นราวปี 2013 ทอม เพรสตัน-เวอร์เนอร์, ผู้ร่วมก่อตั้ง GitHub, ต้องการบางสิ่งที่อ่านง่ายมาก  YAML และ INI เป็นทางเลือก แต่ TOML เหมือนเป็นสิ่งที่ดีที่สุดของทั้งสอง

เฟี้ยวฟ้าว, คุณมีข้อมูลที่ซ้อนกันและอาร์เรย์, ลบยิงเท้าของ YAML และวงเล็บหยิกของ JSON TOML ตอนนี้เป็นไปสำหรับคอนฟิกใน Cargo ของ Rust, ซึ่งพูดถึงการเพิ่มขึ้นของมันในโลกของผู้พัฒนา มันขับเคลื่อนโดยสเปค, ทำให้สิ่งต่างๆ เข้มงวดและกำหนดไว้อย่างดี คุณจะได้เครื่องมือแปลงในแทบจะทุกภาษา, ทำให้มันง่ายต่อการรับรองในวงกว้าง

## ดูเพิ่มเติม
- คลัง GitHub ของ TOML อย่างเป็นทางการ: https://github.com/toml-lang/toml
- toml-cli บน PyPI: https://pypi.org/project/toml-cli/
- เปรียบเทียบรูปแบบการซีเรียไลซ์ข้อมูล: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
