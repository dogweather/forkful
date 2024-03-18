---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:57.647106-06:00
description: "REPL \u0E2B\u0E23\u0E37\u0E2D Read-Eval-Print Loop \u0E40\u0E1B\u0E47\
  \u0E19\u0E2A\u0E20\u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E01\u0E32\
  \u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E41\
  \u0E1A\u0E1A\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E17\u0E35\u0E48\u0E23\u0E31\u0E1A\
  \u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E08\u0E32\u0E01\u0E1C\u0E39\u0E49\u0E43\u0E0A\
  \u0E49\u0E41\u0E15\u0E48\u0E25\u0E30\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07, \u0E14\
  \u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23, \u0E41\u0E25\u0E49\u0E27\u0E2A\
  \u0E48\u0E07\u0E01\u0E25\u0E31\u0E1A\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.646966-06:00'
model: gpt-4-0125-preview
summary: "REPL \u0E2B\u0E23\u0E37\u0E2D Read-Eval-Print Loop \u0E40\u0E1B\u0E47\u0E19\
  \u0E2A\u0E20\u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\
  \u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E41\u0E1A\
  \u0E1A\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E17\u0E35\u0E48\u0E23\u0E31\u0E1A\u0E04\
  \u0E33\u0E2A\u0E31\u0E48\u0E07\u0E08\u0E32\u0E01\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\
  \u0E41\u0E15\u0E48\u0E25\u0E30\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07, \u0E14\u0E33\
  \u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23, \u0E41\u0E25\u0E49\u0E27\u0E2A\u0E48\
  \u0E07\u0E01\u0E25\u0E31\u0E1A\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
REPL หรือ Read-Eval-Print Loop เป็นสภาพแวดล้อมการเขียนโปรแกรมแบบโต้ตอบที่รับคำสั่งจากผู้ใช้แต่ละคำสั่ง, ดำเนินการ, แล้วส่งกลับผลลัพธ์ โปรแกรมเมอร์ใช้มันเพื่อการตอบสนองทันที, การดีบัก, และการทดลองกับแนวคิดการเขียนโค้ดอย่างรวดเร็วโดยไม่มีความจำเป็นต้องคอมไพล์และรันโปรแกรมเต็มรูปแบบ

## วิธีการ:
ใน Fish, shell แบบโต้ตอบเป็นโหมดเริ่มต้นเมื่อคุณเปิดมันขึ้น นี่คือลักษณะที่มันแสดงผล:

```Fish Shell
> set color blue
> echo "ท้องฟ้ามีสี $color"
ท้องฟ้ามีสีน้ำเงิน
```

คุณยังสามารถรันฟังก์ชั่นที่มีอยู่แล้วในระบบและเล่นกับการแทนที่คำสั่ง:

```Fish Shell
> function cheer
      echo "ยินดีต้อนรับ Fish $argv!"
  end
> cheer นักเขียนโค้ด
ยินดีต้อนรับ Fish นักเขียนโค้ด!
```

ไม่เพียงแค่การกำหนดฟังก์ชั่น, คุณสามารถดำเนินการตัดข้อความเล็กๆน้อยๆได้ทันทีและเห็นผลลัพธ์ภายในพริบตา:

```Fish Shell
> math "40 / 2"
20
```

## ดำดิ่งลึก
แนวคิดของ REPLs มีมาตั้งแต่ภาษา Lisp ในช่วงทศวรรษ 1960 การเขียนโปรแกรมแบบโต้ตอบนี้ได้กำหนดมาตรฐานสำหรับสภาพแวดล้อมเช่น `ipython` ของ Python และ `irb` ของ Ruby Fish ได้สืบสานกระแสด้วยความเน้นที่ความเป็นมิตรต่อผู้ใช้และการใช้งานแบบโต้ตอบ

Fish แตกต่างจาก shell อื่นๆ เช่น Bash เพราะได้รับการออกแบบมาให้คำนึงถึงความเป็นโต้ตอบตั้งแต่เริ่มต้น มันให้คุณสมบัติเช่น syntax highlighting, คำแนะนำอัตโนมัติ, และการเติมคำด้วยแท็บที่ทำให้การใช้งานในรูปแบบ REPL ทรงพลังยิ่งขึ้น ยิ่งไปกว่านั้น, คำสั่งของคุณจะถูกจดจำและสามารถค้นหาได้, ทำให้การทดสอบซ้ำๆ เป็นเรื่องง่าย

สิ่งทดแทน REPL ของ Fish อาจเป็น `bash` หรือ `zsh` เมื่อจับคู่กับส่วนขยายเช่น `bash-completion` หรือ `oh-my-zsh`, แต่ Fish มักจะเสนอประสบการณ์ที่สมบูรณ์กว่าเมื่อออกจากกล่อง

## ดูเพิ่มเติม:
- เอกสาร Fish: https://fishshell.com/docs/current/index.html
- การเปรียบเทียบที่น่าสนใจของ Fish กับ shell อื่นๆ: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- การดำดิ่งลึกเข้าไปใน REPLs: https://en.wikipedia.org/wiki/Read–eval–print_loop
- การเขียนโปรแกรมแบบโต้ตอบใน Lisp, มุมมองทางประวัติศาสตร์: http://www.paulgraham.com/ilisp.html
