---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:19.997105-06:00
description: "TOML \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C\u0E04\u0E2D\u0E19\u0E1F\u0E34\u0E01\u0E17\u0E35\u0E48\u0E2D\u0E48\
  \u0E32\u0E19\u0E07\u0E48\u0E32\u0E22\u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\
  \u0E01\u0E21\u0E35\u0E04\u0E27\u0E32\u0E21\u0E0A\u0E31\u0E14\u0E40\u0E08\u0E19\u0E17\
  \u0E32\u0E07\u0E40\u0E0A\u0E34\u0E07\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23 \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\
  \ TOML\u2026"
lastmod: '2024-03-17T21:57:56.751516-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E44\u0E1F\
  \u0E25\u0E4C\u0E04\u0E2D\u0E19\u0E1F\u0E34\u0E01\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\
  \u0E19\u0E07\u0E48\u0E32\u0E22\u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01\
  \u0E21\u0E35\u0E04\u0E27\u0E32\u0E21\u0E0A\u0E31\u0E14\u0E40\u0E08\u0E19\u0E17\u0E32\
  \u0E07\u0E40\u0E0A\u0E34\u0E07\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23 \u0E42\u0E1B\
  \u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49 TOML\
  \ \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\
  \u0E1A\u0E04\u0E2D\u0E19\u0E1F\u0E34\u0E01\u0E41\u0E2D\u0E1B\u0E41\u0E25\u0E30\u0E01\
  \u0E32\u0E23\u0E2D\u0E19\u0E38\u0E01\u0E23\u0E21\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E04\
  \u0E27\u0E32\u0E21\u0E22\u0E38\u0E48\u0E07\u0E22\u0E32\u0E01\u0E02\u0E2D\u0E07 XML\
  \ \u0E2B\u0E23\u0E37\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E09\u0E1E\u0E32\u0E30\
  \u0E40\u0E08\u0E32\u0E30\u0E08\u0E07\u0E02\u0E2D\u0E07 YAML."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## อะไร & ทำไม?

TOML เป็นรูปแบบไฟล์คอนฟิกที่อ่านง่ายเนื่องจากมีความชัดเจนทางเชิงวิธีการ โปรแกรมเมอร์ใช้ TOML เพื่อจัดการกับคอนฟิกแอปและการอนุกรมข้อมูลโดยไม่ต้องใช้ความยุ่งยากของ XML หรือความเฉพาะเจาะจงของ YAML

## วิธีการ:

ก่อนอื่น, ติดตั้ง gem `toml-rb` เป็นตัวเลือกยอดนิยมสำหรับการแยกวิเคราะห์ TOML ใน Ruby

```Ruby
gem install toml-rb
```

ต่อไป, การอ่านไฟล์ TOML:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

ตัวอย่างผลลัพธ์อาจเป็น:

```
My Awesome App
```

การเขียนไปยังไฟล์ TOML:

```Ruby
require 'toml-rb'

config = {
  'title' => 'My Awesome App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

ตรวจสอบ `config.toml` และคุณจะเห็นการตั้งค่าของคุณได้รับการเก็บรักษาอย่างเรียบร้อย

## ดื่มด่ำ

TOML ย่อมาจาก Tom's Obvious, Minimal Language ถูกสร้างโดย Tom Preston-Werner, ผู้ร่วมก่อตั้ง GitHub, ประมาณปี 2013 เป้าหมายหลักคือเพื่อเป็นรูปแบบที่ตรงไปตรงมาและง่ายต่อการแยกวิเคราะห์เข้าสู่โครงสร้างข้อมูล แม้ว่า JSON เป็นที่ยอดเยี่ยมสำหรับ API และ YAML เป็นที่ยืดหยุ่น, แต่ช่องว่างของ TOML คือการเน้นย้ำไปที่ความเป็นมิตรกับมนุษย์ ไม่เหมือนกับ YAML ที่อาจมีความละเอียดอ่อนกับการเยื้อง, TOML มุ่งหมายไปที่โครงสร้างที่คล้ายกับ INI ซึ่งหลายคนพบว่าง่ายขึ้นและน้อยความผิดพลาด

ทางเลือกอื่นๆ เช่น JSON, YAML, หรือ XML ล้วนมีจุดแข็งของตัวเอง แต่ TOML เจริญรุ่งเรืองในสถานการณ์ที่คอนฟิกควรจะง่ายต่อการบำรุงรักษาโดยมนุษย์และโปรแกรมเหมือนกัน ไม่เพียงแต่ง่ายกว่าเท่านั้นแต่ยังบังคับใช้การจัดรูปแบบที่เข้มงวดและอ่านง่าย

ทางด้านเทคนิค, เพื่อแยกวิเคราะห์เนื้อหา TOML ด้วย Ruby, เราใช้ประโยชน์จาก gems เช่น `toml-rb` Gem นี้ใช้ประโยชน์จากธรรมชาติแบบไดนามิกของ Ruby, แปลงข้อมูล TOML เป็นแฮช, อาร์เรย์ Ruby พื้นฐานและโครงสร้างข้อมูลพื้นฐานอื่นๆ การแปลงนี้หมายความว่านักพัฒนาสามารถทำงานกับข้อมูล TOML โดยใช้สัญญาณวิธีและวิธีการที่คุ้นเคยของ Ruby

## ดูเพิ่มเติม

- โปรเจ็กต์และข้อกำหนดของ TOML: https://toml.io/en/
- Gem `toml-rb`: https://github.com/emancu/toml-rb
- เปรียบเทียบ TOML, YAML, และ JSON: https://blog.theodo.com/2021/08/compare-yml-toml-json/
