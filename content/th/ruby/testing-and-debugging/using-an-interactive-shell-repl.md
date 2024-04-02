---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:58.883683-06:00
description: "Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E2B\u0E23\
  \u0E37\u0E2D REPL (Read-Eval-Print Loop) \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\
  \u0E04\u0E38\u0E13\u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E42\u0E04\u0E49\u0E14\u0E41\u0E1A\
  \u0E1A\u0E40\u0E23\u0E35\u0E22\u0E25\u0E44\u0E17\u0E21\u0E4C\u0E44\u0E14\u0E49 \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\
  \u0E21\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E14\u0E25\u0E2D\u0E07 \u0E41\
  \u0E01\u0E49\u0E1A\u0E31\u0E04 \u0E41\u0E25\u0E30\u0E40\u0E23\u0E35\u0E22\u0E19\u0E23\
  \u0E39\u0E49\u0E04\u0E27\u0E32\u0E21\u0E1B\u0E23\u0E30\u0E13\u0E35\u0E15\u0E02\u0E2D\
  \u0E07 Ruby\u2026"
lastmod: '2024-03-17T21:57:56.730951-06:00'
model: gpt-4-0125-preview
summary: "Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E2B\u0E23\
  \u0E37\u0E2D REPL (Read-Eval-Print Loop) \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\
  \u0E04\u0E38\u0E13\u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E42\u0E04\u0E49\u0E14\u0E41\u0E1A\
  \u0E1A\u0E40\u0E23\u0E35\u0E22\u0E25\u0E44\u0E17\u0E21\u0E4C\u0E44\u0E14\u0E49 \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\
  \u0E21\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E14\u0E25\u0E2D\u0E07 \u0E41\
  \u0E01\u0E49\u0E1A\u0E31\u0E04 \u0E41\u0E25\u0E30\u0E40\u0E23\u0E35\u0E22\u0E19\u0E23\
  \u0E39\u0E49\u0E04\u0E27\u0E32\u0E21\u0E1B\u0E23\u0E30\u0E13\u0E35\u0E15\u0E02\u0E2D\
  \u0E07 Ruby\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
weight: 34
---

## อะไร & ทำไม?
Shell แบบโต้ตอบหรือ REPL (Read-Eval-Print Loop) ช่วยให้คุณทดสอบโค้ดแบบเรียลไทม์ได้ โปรแกรมเมอร์ใช้มันเพื่อทดลอง แก้บัค และเรียนรู้ความประณีตของ Ruby โดยไม่ต้องสร้างสคริปต์แบบเต็มรูปแบบ

## วิธีการ:
REPL ของ Ruby เรียกว่า IRB (Interactive Ruby) เริ่มต้นและลองใช้ Ruby ได้ทันทีจากเทอร์มินัลของคุณ:

```Ruby
irb
2.7.0 :001 > puts "Hello, Ruby world!"
สวัสดี, โลกของ Ruby!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## การดำดิ่งลึก
นำเสนอใน Ruby 1.8, IRB เป็นอุปกรณ์หลักสำหรับผู้ใช้ Ruby มันได้รับแรงบันดาลใจจาก shell แบบโต้ตอบของ Lisp และ Python, ผสานการทดลองกับการตอบสนองทันที Alternatives อย่าง Pry มีฟีเจอร์เพิ่มเติม เช่น syntax highlighting และสภาพแวดล้อมการดีบักที่เข้มแข็งยิ่งขึ้น IRB เองนั้นง่าย แต่สามารถขยายฟังก์ชันได้ด้วย gems เช่น 'irbtools' เพื่อขยายความสามารถ วิธีการทำงานของ IRB ในการอ่าน-ประมวลผล-พิมพ์ คือการอ่านแต่ละบรรทัดของข้อมูลนำเข้า ประมวลผลเป็นโค้ด Ruby แล้วจึงพิมพ์ผลลัพธ์ วนรอบกระบวนการนี้จนกว่าจะออก

## ดูเพิ่มเติม
- [IRB ของ Ruby](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [เจม irbtools](https://github.com/janlelis/irbtools)
