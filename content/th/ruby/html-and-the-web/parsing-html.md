---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:36.986092-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19 HTML \u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E42\u0E04\
  \u0E49\u0E14 HTML \u0E2D\u0E2D\u0E01\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\
  \u0E32\u0E43\u0E08\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E41\u0E25\
  \u0E30\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E02\u0E2D\u0E07\u0E21\u0E31\u0E19\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\
  \u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E1B\u0E23\u0E31\u0E1A\u0E41\
  \u0E01\u0E49\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u2026"
lastmod: '2024-03-17T21:57:56.727165-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19 HTML \u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E42\u0E04\
  \u0E49\u0E14 HTML \u0E2D\u0E2D\u0E01\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\
  \u0E32\u0E43\u0E08\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E41\u0E25\
  \u0E30\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E02\u0E2D\u0E07\u0E21\u0E31\u0E19\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\
  \u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E1B\u0E23\u0E31\u0E1A\u0E41\
  \u0E01\u0E49\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32 \u0E2B\u0E23\u0E37\u0E2D\u0E22\
  \u0E49\u0E32\u0E22\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E23\u0E30\u0E2B\u0E27\u0E48\
  \u0E32\u0E07\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E41\u0E25\u0E30\u0E23\u0E30\u0E1A\
  \u0E1A\u0E15\u0E48\u0E32\u0E07\u0E46."
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
weight: 43
---

## วิธีการ:
เพื่อทำการแยกส่วน HTML ใน Ruby, ติดตั้ง 'Nokogiri' gem ด้วยคำสั่ง `gem install nokogiri` Nokogiri เหมือนเป็นมีดสวิสสำหรับการทำงานกับ HTML และ XML ใน Ruby นี่คือตัวอย่างง่ายๆ:

```ruby
require 'nokogiri'
require 'open-uri'

# โหลดเนื้อหา HTML จากเว็บไซต์
html_content = URI.open('http://example.com').read

# แยกส่วน HTML
doc = Nokogiri::HTML(html_content)

# ดึงชื่อเรื่อง
title = doc.xpath('//title').text
puts "ชื่อของหน้านี้คือ: #{title}"
```

สิ่งนี้จะแสดงผลว่า: `ชื่อของหน้านี้คือ: Example Domain`.

## ศึกษาเพิ่มเติม
ย้อนกลับไปในช่วงต้นๆ ของ Ruby, ตัวเลือกสำหรับการแยกส่วน HTML มีจำกัด REXML ซึ่งผนวกมาในตัว แต่ทำงานช้า จากนั้น Hpricot ก็เข้ามา แต่ก็ค่อยๆ หายไป Nokogiri เปิดตัวในปี 2008, ผสมผสานความง่ายของ Hpricot กับความเร็วและพลังของ libxml, ชุดเครื่องมือ XML ที่ได้รับการพิสูจน์แล้ว

ในโลกการแยกส่วน, มีตัวเลือกอื่นๆ อยู่เสมอ บางคนยังคงใช้ไลบรารี 'rexml' ที่มีอยู่ตั้งแต่แรกหรือ 'oga', อีกหนึ่งตัวแยกส่วน XML/HTML สำหรับ Ruby แต่ Nokogiri ยังคงเป็นที่ชื่นชอบด้วยความเข้มแข็งและความเร็วของมัน, ไม่ต้องพูดถึงคุณสมบัติมากมายที่มันมี

ภายใต้ฝาครอบ, Nokogiri แปลง HTML เป็น Document Object Model (DOM)—โครงสร้างแบบต้นไม้ ทำให้ง่ายต่อการนำทางและจัดการกับองค์ประกอบ โดยใช้ XPath และ CSS selectors, คุณสามารถระบุข้อมูลใดๆ ที่คุณต้องการได้อย่างแม่นยำ

## ดูเพิ่มเติม
- Nokogiri gem: [https://nokogiri.org/](https://nokogiri.org/)
- เอกสารของ rexml ของ Ruby: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- ตัวแยกส่วนอื่น 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- เรียนรู้เกี่ยวกับ XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
