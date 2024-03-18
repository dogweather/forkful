---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:16.191207-06:00
description: "\u0E01\u0E32\u0E23 Refactor \u0E04\u0E37\u0E2D\u0E01\u0E23\u0E30\u0E1A\
  \u0E27\u0E19\u0E01\u0E32\u0E23\u0E43\u0E19\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\
  \u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E2D\u0E07\u0E42\u0E04\
  \u0E49\u0E14\u0E04\u0E2D\u0E21\u0E1E\u0E34\u0E27\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E17\
  \u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E40\u0E14\u0E34\u0E21\u0E42\u0E14\
  \u0E22\u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\
  \u0E07\u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E20\u0E32\u0E22\u0E19\u0E2D\
  \u0E01 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E01\u0E32\u0E23 refactor\u2026"
lastmod: '2024-03-17T21:57:56.737592-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23 Refactor \u0E04\u0E37\u0E2D\u0E01\u0E23\u0E30\u0E1A\u0E27\
  \u0E19\u0E01\u0E32\u0E23\u0E43\u0E19\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\
  \u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\
  \u0E14\u0E04\u0E2D\u0E21\u0E1E\u0E34\u0E27\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E17\u0E35\
  \u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E40\u0E14\u0E34\u0E21\u0E42\u0E14\u0E22\
  \u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\
  \u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E01\u0E32\u0E23 refactor\u2026"
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การ Refactor คือกระบวนการในการปรับโครงสร้างของโค้ดคอมพิวเตอร์ที่มีอยู่เดิมโดยไม่เปลี่ยนแปลงพฤติกรรมภายนอก โปรแกรมเมอร์ทำการ refactor เพื่อปรับปรุงคุณสมบัติที่ไม่ใช่ฟังก์ชั่นของซอฟต์แวร์ เช่น การอ่านได้ง่ายขึ้น ลดความซับซ้อน ปรับปรุงการบำรุงรักษา หรือการเพิ่มประสิทธิภาพ

## วิธีการ:

ลองไปดูตัวอย่างของการ refactor วิธีการใน Ruby ที่คำนวณผลรวมของกำลังสอง

**ก่อน Refactoring:**
```ruby
def sum_of_squares(numbers)
  sum = 0
  numbers.each do |number|
    square = number * number
    sum += square
  end
  sum
end

puts sum_of_squares([1, 2, 3])  # ผลลัพธ์: 14
```

**หลัง Refactoring:**
```ruby
def sum_of_squares(numbers)
  numbers.map { |number| number**2 }.sum
end

puts sum_of_squares([1, 2, 3])  # ผลลัพธ์: 14
```

เวอร์ชั่นที่ได้รับการ refactor ใช้ Ruby Enumerables เพื่อแสดงตรรกะเดียวกันได้อย่างกระชับและชัดเจนยิ่งขึ้น วิธีการ `map` ทำการเปลี่ยนแปลงแต่ละรายการ และ `sum` รวมค่าของพวกเขา ลดความจำเป็นในการจัดการ loop และการกำหนดตัวแปรด้วยตนเอง

## ลึกซึ้งยิ่งขึ้น

การ Refactor มีบริบทประวัติศาสตร์ที่ร่ำรวย ย้อนกลับไปถึงแนวปฏิบัติในช่วงต้นของการพัฒนาซอฟต์แวร์ การกล่าวถึงครั้งแรกสามารถตามไปถึงยุค 1990s ด้วยการสนับสนุนที่สำคัญจาก Martin Fowler ในหนังสือของเขา "Refactoring: Improving the Design of Existing Code" ซึ่งเขาได้ให้เอกสารแคตตาล็อกของรูปแบบสำหรับการ refactoring นับตั้งแต่นั้นมา การ refactor กลายเป็นหัวมุมหลักของการปฏิบัติการพัฒนาแบบ agile

เมื่อเราพูดถึงทางเลือกสำหรับการ refactor เราต้องพิจารณาวิธีการอื่น ๆ เช่น 'การเขียนใหม่' ซึ่งคุณทดแทนระบบเก่าบางส่วนหรือทั้งหมด หรือปรับใช้การปฏิบัติเช่น 'การทบทวนโค้ด' และ 'การเขียนโปรแกรมคู่' เพื่อยกระดับคุณภาพโค้ดอย่างค่อยเป็นค่อยไป อย่างไรก็ตาม สิ่งเหล่านี้ไม่ใช่การแทนที่การ refactor; พวกมันเสริมกระบวนการ

ในแง่ของการนำไปใช้ รูบี้มีไวยากรณ์ที่ยอดเยี่ยมและสื่อความหมายได้อย่างชัดเจน ซึ่งมักจะส่งผลให้โค้ดที่สั้นกว่า อ่านง่ายขึ้นหลังจากการ refactor หลักการหลัก ได้แก่ DRY (Don't Repeat Yourself), การใช้ชื่อที่มีความหมาย, การทำให้วิธีการสั้นและมุ่งเน้นไปที่งานเดียว, และการใช้โมดูล Enumerable ของรูบี้อย่างมีประสิทธิภาพ เช่นที่เห็นในตัวอย่างข้างต้น เครื่องมืออัตโนมัติเช่น RuboCop ยังสามารถช่วยโปรแกรมเมอร์ในการระบุจุดในโค้ดที่สามารถได้รับประโยชน์จากการ refactor

## ดูเพิ่มเติม

เพื่อขุดลึกเพิ่มเติมเกี่ยวกับการ refactor ใน Ruby, ตรวจสอบทรัพยากรเหล่านี้:

- หนังสือสำคัญของ Martin Fowler: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- คู่มือสไตล์ของ Ruby สำหรับการเขียนโค้ดที่สะอาดขึ้น: [The Ruby Style Guide](https://rubystyle.guide/)
- RuboCop, วิเคราะห์โค้ดแบบคงที่ (linter) และจัดรูปแบบ: [RuboCop GitHub Repository](https://github.com/rubocop/rubocop)
