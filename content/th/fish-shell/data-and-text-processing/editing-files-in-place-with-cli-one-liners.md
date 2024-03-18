---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:20.856433-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E44\u0E1F\u0E25\u0E4C\
  \u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u0E14\u0E49\u0E27\u0E22 CLI \u0E04\u0E33\u0E2A\
  \u0E31\u0E48\u0E07\u0E40\u0E14\u0E35\u0E22\u0E27\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E17\u0E33\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\
  \u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E08\u0E32\u0E01 command\
  \ line \u0E42\u0E14\u0E22\u0E15\u0E23\u0E07 \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\
  \u0E15\u0E49\u0E2D\u0E07\u0E40\u0E1B\u0E34\u0E14\u0E43\u0E19\u0E15\u0E31\u0E27\u0E41\
  \u0E01\u0E49\u0E44\u0E02\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u2026"
lastmod: '2024-03-17T21:57:56.641371-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E44\u0E1F\u0E25\u0E4C\
  \u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u0E14\u0E49\u0E27\u0E22 CLI \u0E04\u0E33\u0E2A\
  \u0E31\u0E48\u0E07\u0E40\u0E14\u0E35\u0E22\u0E27\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E17\u0E33\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\
  \u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E08\u0E32\u0E01 command\
  \ line \u0E42\u0E14\u0E22\u0E15\u0E23\u0E07 \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\
  \u0E15\u0E49\u0E2D\u0E07\u0E40\u0E1B\u0E34\u0E14\u0E43\u0E19\u0E15\u0E31\u0E27\u0E41\
  \u0E01\u0E49\u0E44\u0E02\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u2026"
title: "\u0E41\u0E01\u0E49\u0E44\u0E02\u0E44\u0E1F\u0E25\u0E4C\u0E43\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E14\u0E34\u0E21\u0E14\u0E49\u0E27\u0E22 CLI one-liners"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การแก้ไขไฟล์โดยตรงด้วย CLI คำสั่งเดียวหมายถึงการทำการเปลี่ยนแปลงไฟล์จาก command line โดยตรง โดยไม่ต้องเปิดในตัวแก้ไขข้อความ นักโปรแกรมมิ่งทำเช่นนี้เพื่อประหยัดเวลาและอัตโนมัติงานแก้ไขที่ซ้ำๆ เพื่อให้กระบวนการทำงานของพวกเขาเรียบลื่นและมีประสิทธิภาพมากขึ้น

## วิธีการ:

Fish Shell ซึ่งเป็นที่รู้จักกันดีในเรื่องคุณสมบัติที่ใช้งานง่ายและความสามารถในการสคริปต์ที่ทรงพลัง นำเสนอวิธีการหลายวิธีในการแก้ไขไฟล์โดยตรง อย่างไรก็ตาม ไม่เหมือนกับ shells อื่นๆ Fish ไม่มีกลไกในตัวสำหรับการแก้ไขโดยตรง (`sed -i` ใน Bash ตัวอย่างเช่น) แต่ไม่ต้องกังวล คุณยังสามารถทำสิ่งนี้ได้ด้วยการสร้างสรรค์เล็กน้อยและความช่วยเหลือจากเครื่องมือภายนอกเช่น `sed` และ `awk`

### การใช้ `sed` สำหรับการแทนที่ง่ายๆ
เพื่อแทนที่ "hello" ด้วย "world" ใน `file.txt` คุณจะใช้:
```Fish Shell
sed -i '' 's/hello/world/g' file.txt
```

### การใช้งานหลายคำสั่ง `sed`
หากคุณต้องการทำการแทนที่หลายครั้ง คุณสามารถเชื่อมต่อสิ่งเหล่านี้ได้ดังนี้:
```Fish Shell
sed -i '' -e 's/fish/bass/g' -e 's/rainbow/trout/g' file.txt
```

### การใช้ `awk` สำหรับการดำเนินงานที่ซับซ้อนยิ่งขึ้น
สำหรับการดำเนินการที่ซับซ้อนเกินกว่าที่ `sed` จะรับมือได้ `awk` อาจเป็นเครื่องมือที่คุณเลือก นี่คือวิธีการทวีคูณตัวเลขบนแต่ละบรรทัด:
```Fish Shell
awk '{print $1 * 2}' file.txt > temp && mv temp file.txt
```

### หมายเหตุเกี่ยวกับการจัดการข้อผิดพลาด
ควรจำไว้ว่า เมื่อใช้เครื่องมือเหล่านี้จาก Fish การจับข้อผิดพลาดและการเข้าใจข้อความของพวกเขาเป็นสิ่งสำคัญ ใช้การจัดการข้อผิดพลาดที่แข็งแกร่งของ Fish เพื่อทำให้สคริปต์ของคุณน่าเชื่อถือยิ่งขึ้น

## ภายในลึก

ในอดีต การแก้ไขไฟล์โดยตรงเป็นส่วนสำคัญของการเขียนโปรแกรม Unix และ Linux ซึ่งนำเสนอวิธีที่มีประสิทธิภาพในการทำการแก้ไขอย่างรวดเร็วโดยไม่ต้องเปิดไฟล์ด้วยตนเอง เครื่องมือเช่น `sed` และ `awk` เป็นยูทิลิตี้ที่เคารพนับถือซึ่งมีอยู่ตั้งแต่ยุคแรกๆ ของ Unix กลายเป็นสิ่งจำเป็นสำหรับงานประมวลผลข้อความ

Fish Shell ในขณะที่ทันสมัยและมีการปรับปรุงในเรื่องการใช้งานและการเขียนสคริปต์ ขาดการแก้ไขโดยตรงในตัวหลักๆ เนื่องจากปรัชญาการออกแบบที่เน้นการโต้ตอบและความใช้งานง่ายของมัน การขาดคำสั่งแก้ไขโดยตรงใน Fish ย้ำเน้นถึงความสำคัญของเครื่องมือภายนอกในระบบ Unix-like

ทางเลือกสำหรับการแก้ไขไฟล์โดยตรงใน Fish รวมถึงการใช้ไฟล์ชั่วคราวหรือใช้งานคำสั่งเดี่ยวของ Perl หรือ Python ซึ่งสามารถเสนอความยืดหยุ่นหรืออ่านง่ายมากขึ้นสำหรับงานที่ซับซ้อน

ตัวอย่างเช่น การใช้งาน Perl:
```Fish Shell
perl -pi -e 's/find/replace/g' file.txt
```
หรือ Python:
```Fish Shell
python -c "import re, sys; [sys.stdout.write(re.sub('pattern', 'replacement', line)) for line in sys.stdin]" < file.txt > temp && mv temp file.txt
```

ในแง่ของการดำเนินการ เมื่อคุณทำการแก้ไขโดยตรง โดยภายในเครื่องมือเหล่านี้มักจะสร้างไฟล์ชั่วคราว เขียนการเปลี่ยนแปลงไว้ที่นั่น แล้วจึงแทนที่ไฟล์ต้นฉบับด้วยเวอร์ชันที่มีการแก้ไข วิธีนี้ช่วยให้แน่ใจว่ากระบวนการแก้ไขไฟล์ไม่ทำให้ข้อมูลเสียหายหรือสูญหายหากเกิดข้อผิดพลาดระหว่างการดำเนินการ

การเข้าใจเครื่องมือและวิธีการเหล่านี้ช่วยให้โปรแกรมเมอร์ Fish Shell สามารถรวมการแก้ไขไฟล์โดยตรงเข้ากับสคริปต์ของพวกเขาได้อย่างมีประสิทธิผล เชื่อมโยงระหว่างคุณสมบัติที่ใช้งานง่ายของ Fish และพลังสายพันธุ์ดั้งเดิมของเครื่องมือประมวลผลข้อความ Unix