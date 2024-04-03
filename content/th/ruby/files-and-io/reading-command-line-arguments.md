---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:59.989885-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E43\u0E19\u0E01\u0E32\u0E23\
  \u0E08\u0E31\u0E1A\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\
  \u0E4C\u0E08\u0E32\u0E01\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\u0E33\u0E2A\u0E31\
  \u0E48\u0E07 Ruby \u0E21\u0E35\u0E2D\u0E32\u0E23\u0E4C\u0E40\u0E23\u0E22\u0E4C\u0E07\
  \u0E48\u0E32\u0E22 \u0E46 \u0E43\u0E2B\u0E49\u0E43\u0E0A\u0E49: `ARGV` \u0E21\u0E31\
  \u0E19\u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\u0E14\u0E49\u0E27\u0E22\u0E2D\u0E32\u0E23\
  \u0E4C\u0E01\u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E17\u0E31\u0E49\u0E07\u0E2B\
  \u0E21\u0E14\u0E17\u0E35\u0E48\u0E16\u0E39\u0E01\u0E2A\u0E48\u0E07\u0E21\u0E32\u0E43\
  \u0E19\u0E25\u0E33\u0E14\u0E31\u0E1A\u0E17\u0E35\u0E48\u0E43\u0E2B\u0E49\u0E44\u0E27\
  \u0E49."
lastmod: '2024-03-17T21:57:56.744107-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19\u0E01\u0E32\u0E23\u0E08\u0E31\u0E1A\u0E2D\u0E32\u0E23\u0E4C\
  \u0E01\u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E1A\u0E23\u0E23\
  \u0E17\u0E31\u0E14\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 Ruby \u0E21\u0E35\u0E2D\u0E32\
  \u0E23\u0E4C\u0E40\u0E23\u0E22\u0E4C\u0E07\u0E48\u0E32\u0E22 \u0E46 \u0E43\u0E2B\
  \u0E49\u0E43\u0E0A\u0E49."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
weight: 23
---

## วิธีทำ:
ในการจับอาร์กิวเมนต์จากบรรทัดคำสั่ง Ruby มีอาร์เรย์ง่าย ๆ ให้ใช้: `ARGV` มันประกอบด้วยอาร์กิวเมนต์ทั้งหมดที่ถูกส่งมาในลำดับที่ให้ไว้

```Ruby
# hello.rb
name = ARGV[0] || "World"
puts "Hello, #{name}!"

# รันด้วย: ruby hello.rb Alice
# ผลลัพธ์: Hello, Alice!
```

ในการจัดการกับอาร์กิวเมนต์หลายตัว:

```Ruby
# greet.rb
name, time_of_day = ARGV
puts "Good #{time_of_day || 'day'}, #{name || 'there'}!"

# รันด้วย: ruby greet.rb Bob Morning
# ผลลัพธ์: Good Morning, Bob!
```

สร้างตัวเลือกโดยใช้ลูป:

```Ruby
# options.rb
options = {}
ARGV.each do |arg|
  key, value = arg.split('=')
  options[key.to_sym] = value
end
p options

# รันด้วย: ruby options.rb name=Tom age=30
# ผลลัพธ์: {:name=>"Tom", :age=>"30"}
```

## ลงลึก
การอ่านอาร์กิวเมนต์จากบรรทัดคำสั่งเป็นปฏิบัติการที่เก่าแก่เท่ากับอินเตอร์เฟซบรรทัดคำสั่งเอง มันเกี่ยวกับการใช้ประโยชน์จากการป้อนข้อมูลของผู้ใช้โดยไม่มี GUI—จำเป็นสำหรับการอัตโนมัติหรือเมื่อรันสคริปต์บนเซิร์ฟเวอร์

`ARGV` ของ Ruby ไม่ใช่สิ่งที่แปลกใหม่; ภาษามากมายมีบางอย่างที่คล้ายคลึงกัน อย่างไรก็ตาม การดำเนินการของ Ruby นั้นเน้นที่ความเรียบง่ายและไวยากรณ์ที่ชัดเจน—ไม่มีความวุ่นวาย เพียงแค่อาร์เรย์

ใต้พื้นผิว, `ARGV` เป็นเพียงอินสแตนซ์ของ `Array` ที่เตรียมพร้อมด้วยอาร์กิวเมนต์ที่ปรากฏหลังชื่อสคริปต์ในการเรียกคำสั่ง Ruby ตั้งค่ามันขึ้นก่อนโค้ดของคุณถึงจะรัน ทำให้มันพร้อมใช้งานได้ทันที

มีทางเลือกหรือ? แน่นอน สำหรับความต้องการที่ซับซ้อน เช่น การแยกวิเคราะห์ธง (เช่น `--verbose` หรือ `-v`), Ruby มีคลาส `OptionParser` ในไลบรารีมาตรฐาน นี่สามารถจัดการได้มากกว่า `ARGV` เช่น ค่าเริ่มต้น การเปลี่ยนแปลงประเภทอัตโนมัติ และการสร้างข้อความช่วยเหลือ

บางครั้ง คุณอาจต้องการทราบว่ามีการให้อาร์กิวเมนต์หรือไม่ โดยไม่สนใจค่าของมัน สำหรับเรื่องนี้ `ARGV.include?` ช่วยได้

## อ่านเพิ่มเติม
- บทนำถึง `OptionParser`: [https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html)
- เพิ่มเติมเกี่ยวกับอาร์กิวเมนต์บรรทัดคำสั่งใน Ruby: [https://www.rubyguides.com/2018/12/ruby-argv/](https://www.rubyguides.com/2018/12/ruby-argv/)
