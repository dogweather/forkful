---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:26.338069-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E40\u0E19\u0E37\u0E49\
  \u0E2D\u0E2B\u0E32 HTML \u0E08\u0E32\u0E01\u0E2D\u0E34\u0E19\u0E40\u0E17\u0E2D\u0E23\
  \u0E4C\u0E40\u0E19\u0E47\u0E15 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25, \u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  ,\u2026"
lastmod: '2024-03-17T21:57:56.728187-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E40\u0E19\u0E37\u0E49\
  \u0E2D\u0E2B\u0E32 HTML \u0E08\u0E32\u0E01\u0E2D\u0E34\u0E19\u0E40\u0E17\u0E2D\u0E23\
  \u0E4C\u0E40\u0E19\u0E47\u0E15 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\
  \u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25, \u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  ,\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การดาวน์โหลดเว็บเพจหมายถึงการเข้าถึงเนื้อหา HTML จากอินเทอร์เน็ต โปรแกรมเมอร์ทำเช่นนี้เพื่อวิเคราะห์ข้อมูล, ดึงข้อมูล, หรือติดตามการเปลี่ยนแปลงโดยใช้โปรแกรม

## วิธีการ:
Ruby ทำให้การดาวน์โหลดเว็บเพจเป็นเรื่องง่ายด้วยไลบรารีเช่น `net/http` และ gems เช่น `open-uri` นี่คือวิธีการทำโดยใช้ `net/http`:

```Ruby
require 'net/http'
require 'uri'

url = URI.parse('http://example.com') 
response = Net::HTTP.get_response(url)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

คุณจะได้รับเนื้อหา HTML ของ `http://example.com` ที่พิมพ์ออกมา

การใช้ `open-uri` นั้นง่ายขึ้นอีก:

```Ruby
require 'open-uri'

downloaded_page = URI.open('http://example.com').read
puts downloaded_page
```

อีกครั้ง, เนื้อหาของเว็บเพจจะถูกแสดงบนเทอมินัลของคุณ

## การศึกษาอย่างลึกซึ้ง
ในช่วงวันแรกๆ ของเว็บ, การดาวน์โหลดหน้าเว็บลำบากกว่านี้ โดยต้องทำการสร้างคำขอ HTTP ด้วยตนเอง ปัจจุบัน, Ruby ลดความซับซ้อนของเรื่องนี้ลงมาก

ตัวเลือกอื่นที่ไม่ใช่ `net/http` และ `open-uri` รวมถึง gems ที่ระดับสูงกว่า เช่น `HTTParty` และ `RestClient` พวกเขาเสนอคุณสมบัติเพิ่มเติมและวิธีการเชิงวัตถุ สำหรับการดึงข้อมูลเว็บอย่างหนัก, หลายคนในวงการ Ruby หันไปใช้ `Nokogiri` เพื่อวิเคราะห์ HTML หรือ `Mechanize` ซึ่งทำงานเหมือนเบราว์เซอร์เว็บ

เมื่อมาถึงการทำงาน, ควรจำไว้ว่า `open-uri` เป็นห่อหุ้มสำหรับ `net/http`, ดังนั้นมันจึงสะดวกมาก แต่อาจขาดการควบคุมระดับต่ำบ้าง `net/http` ให้ความควบคุมเพิ่มเติมแต่อาจมีคำศัพท์มากเกินไปสำหรับงานง่ายๆ

## ดูเพิ่มเติม
สำหรับการอ่านเพิ่มเติมและทรัพยากรเพิ่มเติม ตรวจสอบที่:

- เอกสารของ Ruby Net::HTTP: [https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- เอกสาร Open-URI: [https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html)
- เว็บเพจของ Nokogiri: [https://nokogiri.org/](https://nokogiri.org/)
- ที่เก็บ gem ของ Mechanize: [https://github.com/sparklemotion/mechanize](https://github.com/sparklemotion/mechanize)
- Gem HTTParty บน GitHub: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
- Gem RestClient: [https://github.com/rest-client/rest-client](https://github.com/rest-client/rest-client)
