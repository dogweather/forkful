---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:19.024724-06:00
description: "\u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E43\u0E0A\u0E49 regular expressions (regex) \u0E43\u0E19 Fish\
  \ Shell \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E04\u0E49\u0E19\u0E2B\u0E32, \u0E08\u0E31\
  \u0E1A\u0E04\u0E39\u0E48, \u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E15\u0E32\u0E21\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\
  \u0E40\u0E09\u0E1E\u0E32\u0E30 \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49 regex\u2026"
lastmod: '2024-03-17T21:57:56.633924-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E43\u0E0A\u0E49 regular expressions (regex) \u0E43\u0E19 Fish\
  \ Shell \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E04\u0E49\u0E19\u0E2B\u0E32, \u0E08\u0E31\
  \u0E1A\u0E04\u0E39\u0E48, \u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\
  \u0E2A\u0E15\u0E23\u0E34\u0E07\u0E15\u0E32\u0E21\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\
  \u0E40\u0E09\u0E1E\u0E32\u0E30 \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49 regex \u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E40\u0E0A\u0E48\u0E19\u0E01\u0E32\
  \u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E1B\u0E49\u0E2D\u0E19\u0E40\u0E02\u0E49\u0E32 (input validation), \u0E01\u0E32\
  \u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C (parsing),\
  \ \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\
  \u0E25\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21 \u0E40\u0E1E\u0E23\u0E32\u0E30\u0E21\
  \u0E31\u0E19\u0E40\u0E2A\u0E19\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E01\
  \u0E23\u0E30\u0E0A\u0E31\u0E1A\u0E41\u0E25\u0E30\u0E17\u0E23\u0E07\u0E1E\u0E25\u0E31\
  \u0E07\u0E43\u0E19\u0E01\u0E32\u0E23\u0E23\u0E30\u0E1A\u0E38\u0E23\u0E39\u0E1B\u0E41\
  \u0E1A\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E0B\u0E31\
  \u0E1A\u0E0B\u0E49\u0E2D\u0E19."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E23\u0E01\u0E38\u0E25\u0E32\u0E23\
  \u0E4C\u0E40\u0E2D\u0E47\u0E01\u0E40\u0E1E\u0E23\u0E2A\u0E0A\u0E31\u0E19"
weight: 11
---

## อะไรและทำไม?

นักพัฒนาโปรแกรมใช้ regular expressions (regex) ใน Fish Shell เพื่อค้นหา, จับคู่, และจัดการสตริงตามรูปแบบเฉพาะ โดยใช้ regex สำหรับงานเช่นการตรวจสอบข้อมูลป้อนเข้า (input validation), การแยกวิเคราะห์ (parsing), และการประมวลผลข้อความ เพราะมันเสนอวิธีที่กระชับและทรงพลังในการระบุรูปแบบข้อความที่ซับซ้อน

## วิธีการ:

แม้ Fish Shell เองไม่มีคำสั่งภายในสำหรับ regex, แต่มันใช้ประโยชน์จากคำสั่งภายนอกเช่น `grep`, `sed`, และ `awk` ที่รองรับ regex, ทำให้คุณสามารถรวมการดำเนินการ regex ในสคริปต์ของคุณได้

### การจับคู่รูปแบบพื้นฐานด้วย `grep`
ค้นหาบรรทัดในไฟล์ที่ตรงกับรูปแบบ:

```fish
grep '^[0-9]+' myfile.txt
```

คำสั่งนี้หาบรรทัดที่เริ่มต้นด้วยหนึ่งหรือมากกว่าหนึ่งตัวเลขใน `myfile.txt`

### การสกัดและการแทนที่ด้วย `sed`
สกัดหมายเลขโทรศัพท์ออกจากไฟล์:

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

แทนที่ทุกคำว่า "foo" ด้วย "bar" ใน `data.txt`:

```fish
sed 's/foo/bar/g' data.txt
```

### การใช้ `string` สำหรับ Regex พื้นฐาน
คำสั่ง `string` ของ Fish Shell รองรับการดำเนินการ regex พื้นฐานเช่นการจับคู่และการแทนที่:

จับคู่รูปแบบในสตริง:

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
ผลลัพธ์:
```
3.1.2
```

แทนที่ตัวเลขหลังจาก 'fish' ด้วย 'X.X.X':

```fish
echo "Welcome to fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
ผลลัพธ์:
```
Welcome to fish X.X.X
```

### การจับคู่ขั้นสูงด้วย `awk`
พิมพ์คอลัมน์ที่สองของข้อมูลที่คอลัมน์แรกตรงกับรูปแบบเฉพาะ:

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

คำสั่งนี้ค้นหาบรรทัดใน `datafile` ที่คอลัมน์แรกเริ่มต้นด้วยตัว "a" ตามด้วยหนึ่งหรือมากกว่าหนึ่งตัวเลข และพิมพ์คอลัมน์ที่สอง

ด้วยการรวมคำสั่งภายนอกเหล่านี้, นักพัฒนาโปรแกรม Fish Shell สามารถใช้ประโยชน์จากพลังของ regular expressions สำหรับงานปรับแต่งข้อความที่ซับซ้อนได้อย่างเต็มที่, เพิ่มความสามารถของ shell อย่างมีนัยสำคัญ
