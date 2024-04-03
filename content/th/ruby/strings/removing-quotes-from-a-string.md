---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:49.309582-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Ruby \u0E21\u0E35\u0E40\u0E17\
  \u0E04\u0E19\u0E34\u0E04\u0E14\u0E35\u0E46 \u0E43\u0E19\u0E01\u0E32\u0E23\u0E15\u0E31\
  \u0E14\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E2D\u0E31\
  \u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\u0E17\u0E35\u0E48\u0E19\u0E48\u0E32\u0E23\
  \u0E33\u0E04\u0E32\u0E0D\u0E40\u0E2B\u0E25\u0E48\u0E32\u0E19\u0E35\u0E49\u0E2D\u0E2D\
  \u0E01 \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E40\
  \u0E21\u0E18\u0E2D\u0E14 `gsub` \u0E2B\u0E23\u0E37\u0E2D `delete` \u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E17\u0E33\u0E07\u0E32\u0E19\u0E19\u0E35\u0E49 \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E42\u0E04\u0E49\u0E14\u0E43\u0E2B\u0E49\u0E28\u0E36\u0E01\u0E29\u0E32\
  ."
lastmod: '2024-03-17T21:57:56.715862-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u0E21\u0E35\u0E40\u0E17\u0E04\u0E19\u0E34\u0E04\u0E14\u0E35\u0E46\
  \ \u0E43\u0E19\u0E01\u0E32\u0E23\u0E15\u0E31\u0E14\u0E40\u0E04\u0E23\u0E37\u0E48\
  \u0E2D\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\
  \u0E28\u0E17\u0E35\u0E48\u0E19\u0E48\u0E32\u0E23\u0E33\u0E04\u0E32\u0E0D\u0E40\u0E2B\
  \u0E25\u0E48\u0E32\u0E19\u0E35\u0E49\u0E2D\u0E2D\u0E01 \u0E04\u0E38\u0E13\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14 `gsub`\
  \ \u0E2B\u0E23\u0E37\u0E2D `delete` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E07\
  \u0E32\u0E19\u0E19\u0E35\u0E49 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E42\u0E04\u0E49\
  \u0E14\u0E43\u0E2B\u0E49\u0E28\u0E36\u0E01\u0E29\u0E32."
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\u0E2D\u0E2D\
  \u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 9
---

## วิธีการ:
Ruby มีเทคนิคดีๆ ในการตัดเครื่องหมายอัญประกาศที่น่ารำคาญเหล่านี้ออก คุณสามารถใช้เมธอด `gsub` หรือ `delete` เพื่อทำงานนี้ นี่คือโค้ดให้ศึกษา:

```ruby
# ใช้ gsub เพื่อลบเครื่องหมายอัญประกาศคู่และเดี่ยว
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# ผลลัพธ์: Say hello to my little friend!

# ถ้าคุณรู้ว่าคุณจะจัดการเพียงแค่หนึ่งประเภทของเครื่องหมายอัญประกาศ
single_quoted_string = "'Stay a while and listen!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# ผลลัพธ์: Stay a while and listen!
```

## ล้ำลึก
ประวัติศาสตร์ของเครื่องหมายอัญประกาศย้อนกลับไปถึงวันแรกๆ ของการเขียนโปรแกรม ซึ่งมักถูกใช้เป็นตัวกำหนดสตริง ในปัจจุบันเช่นกัน คุณอาจพบว่าตัวเองต้องการลบเครื่องหมายอัญประกาศเหล่านี้เมื่อไม่จำเป็น หรือเมื่ออาจจะขัดขวางการเก็บข้อมูลและการดำเนินการกับข้อมูล

เราได้พูดถึง `gsub` และ `delete` แต่ยังมีเมธอดอื่นๆ ด้วย อย่างเช่น `tr` หรือ `tr_s` ซึ่งให้ความควบคุมเพิ่มเติมหรือสามารถจัดการกับกรณีการใช้งานที่แตกต่างกัน:

```ruby
# tr สามารถลบเครื่องหมายอัญประกาศได้เช่นกัน
double_quoted_string = "\"Do or do not, there is no try.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# ผลลัพธ์: Do or do not, there is no try.
```

โปรดจำไว้ว่า แต่ละเมธอดนี้มีกรณีการใช้งานของมัน `gsub` มีประสิทธิภาพมากเมื่อคุณกำลังจัดการกับรูปแบบที่ซับซ้อนหรือการแทนที่หลายอย่าง `delete` และ `tr` ใช้ได้ดีสำหรับการลบอักขระที่เรียบง่ายและตรงไปตรงมา

## ดูเพิ่มเติม
สำหรับการอ่านเพิ่มเติม และเพื่อดูเมธอดเหล่านี้ทำงานภายในฐานโค้ดที่ใหญ่กว่า ตรวจสอบที่:
- เอกสาร Ruby สำหรับ [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete), และ [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr).
- Ruby Monstas มีชุดฝึกหัด [String](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html) ที่ยอดเยี่ยม ซึ่งรวมถึงการทำงานกับเครื่องหมายอัญประกาศ
- การอภิปรายใน Stack Overflow เกี่ยวกับ [การจัดการสตริง](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string) ให้ปัญหาและวิธีแก้ไขจาก Rubyists ในโลกแห่งความจริง
