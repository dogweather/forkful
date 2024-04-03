---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:58.672044-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E25\u0E2D\u0E07\u0E1E\
  \u0E34\u0E08\u0E32\u0E23\u0E13\u0E32\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C Bash\
  \ \u0E07\u0E48\u0E32\u0E22\u0E46 \u0E17\u0E35\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E01\u0E32\u0E23 Refactoring \u0E21\u0E31\u0E19\u0E2D\u0E36\u0E14\u0E2D\
  \u0E31\u0E14, \u0E21\u0E35\u0E42\u0E04\u0E49\u0E14\u0E17\u0E35\u0E48\u0E0B\u0E49\
  \u0E33\u0E0B\u0E32\u0E01\u0E41\u0E25\u0E30\u0E22\u0E32\u0E01\u0E15\u0E48\u0E2D\u0E01\
  \u0E32\u0E23\u0E15\u0E34\u0E14\u0E15\u0E32\u0E21."
lastmod: '2024-03-17T21:57:56.405568-06:00'
model: gpt-4-0125-preview
summary: "\u0E25\u0E2D\u0E07\u0E1E\u0E34\u0E08\u0E32\u0E23\u0E13\u0E32\u0E2A\u0E04\
  \u0E23\u0E34\u0E1B\u0E15\u0E4C Bash \u0E07\u0E48\u0E32\u0E22\u0E46 \u0E17\u0E35\u0E48\
  \u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E01\u0E32\u0E23 Refactoring \u0E21\u0E31\
  \u0E19\u0E2D\u0E36\u0E14\u0E2D\u0E31\u0E14, \u0E21\u0E35\u0E42\u0E04\u0E49\u0E14\
  \u0E17\u0E35\u0E48\u0E0B\u0E49\u0E33\u0E0B\u0E32\u0E01\u0E41\u0E25\u0E30\u0E22\u0E32\
  \u0E01\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E15\u0E34\u0E14\u0E15\u0E32\u0E21."
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
weight: 19
---

## วิธีการ:
ลองพิจารณาสคริปต์ Bash ง่ายๆ ที่ต้องการการ Refactoring มันอึดอัด, มีโค้ดที่ซ้ำซากและยากต่อการติดตาม:

```Bash
#!/bin/bash
echo "Enter a filename:"
read filename
if [ -f "$filename" ]; then
    echo "File exists."
    count=$(grep -c "foo" "$filename")
    echo "The word foo appears $count times."
else
    echo "File does not exist."
fi
```

การ Refactoring สำหรับความชัดเจนและความสามารถในการใช้ซ้ำอาจเกี่ยวข้องกับการนำฟังก์ชั่นเข้ามาใช้และการจัดการข้อผิดพลาดได้อย่างมีเมตตากรุณามากขึ้น:

```Bash
#!/bin/bash

function file_exists() {
    [[ -f "$1" ]]
}

function count_occurrences() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "Enter a filename:"
    read -r filename
    echo "Enter the word to search for:"
    read -r word

    if file_exists "$filename"; then
        count=$(count_occurrences "$word" "$filename")
        echo "The word $word appears $count times."
    else
        echo "File does not exist." >&2
        exit 1
    fi
}

main "$@"
```

เวอร์ชันที่ได้รับการ Refactoring ใช้ฟังก์ชั่นเพื่อปรับปรุงความอ่านง่ายและทำให้สามารถนำกลับมาใช้ใหม่ได้

## การไขลึก:
การ Refactoring ไม่ใช่ความคิดที่เริ่มต้นด้วย Bash หรือแม้กระทั่งภาษาโปรแกรมระดับสูง; มันเก่าแก่เท่ากับการเขียนโปรแกรมเอง Refactoring เป็นคำที่ถูกรูปแบบในหนังสือ "Refactoring: Improving the Design of Existing Code" โดย Martin Fowler ในปี 1999, โดยเน้นหลักๆ ที่ภาษาเชิงวัตถุ

ในบริบทของการเขียนสคริปต์ Bash, การ Refactoring มักหมายถึงการแยกสคริปต์ยาวออกเป็นฟังก์ชั่น, ลดการทำซ้ำกับลูปหรือเงื่อนไข, และหลีกเลี่ยงข้อผิดพลาดทั่วไปเช่นการล้มเหลวในการจัดการกับช่องว่างในชื่อไฟล์ การเลือกใช้ทางเลือกอื่นนอกเหนือจาก Bash สำหรับสคริปต์ที่กลายเป็นซับซ้อนเกินไป ได้แก่ Python หรือ Perl ซึ่งมีโครงสร้างข้อมูลและการจัดการข้อผิดพลาดที่ดีกว่าสำหรับงานที่ซับซ้อน

การ Refactoring โดยเฉพาะกับ Bash นั้นเกี่ยวกับการยึดถือแนวทางปฏิบัติที่ดีที่สุด เช่น การอ้างอิงตัวแปร, การใช้ `[[ ]]` สำหรับการทดสอบมากกว่า `[ ]`, และการแนะนำให้ใช้ `printf` มากกว่า `echo` เพื่อผลลัพธ์ที่เข้มแข็ง รายละเอียดการปฏิบัติมักจะเกี่ยวข้องกับการยึดถือตามแนวทางและใช้เครื่องมือเช่น `shellcheck` สำหรับการวิเคราะห์สถิติเพื่อจับข้อผิดพลาดทั่วไป

## ดูเพิ่มเติม:
- [Google's Shell Style Guide](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, เครื่องมือวิเคราะห์สถิติสำหรับสคริปต์ shell](https://www.shellcheck.net/)
- [ศิลปะของ Command Line](https://github.com/jlevy/the-art-of-command-line)
