---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:28.997617-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E07\u0E48\u0E32\u0E22\u0E46\u0E43\u0E19\u0E01\
  \u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\
  \u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\u0E47\u0E01\u0E43\
  \u0E19 Bash."
lastmod: '2024-03-17T21:57:56.382982-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E07\u0E48\
  \u0E32\u0E22\u0E46\u0E43\u0E19\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E40\u0E25\u0E47\u0E01\u0E43\u0E19 Bash."
title: "\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E47\u0E01"
weight: 4
---

## วิธีการ:
นี่คือวิธีง่ายๆในการแปลงสตริงเป็นตัวพิมพ์เล็กใน Bash:

```Bash
str="Make Me Lower Case"
lower_str=$(echo "$str" | tr '[:upper:]' '[:lower:]')

echo $lower_str
```

ผลลัพธ์:

```
make me lower case
```

Bash 4.0 ขึ้นไปมีวิธีในตัวด้วยการขยายพารามิเตอร์:

```Bash
str="Make Me Lower Case"
lower_str="${str,,}"

echo $lower_str
```

ผลลัพธ์:

```
make me lower case
```

## ลงลึก
ก่อน Bash 4.0, วิธีที่ใช้กันทั่วไปในการแปลงสตริงเป็นตัวพิมพ์เล็กมีการใช้ยูทิลิตีภายนอกเช่น `tr`, `awk`, หรือ `sed` แต่ละตัวมีวิธีที่แตกต่างกันในการจัดการสตริงนอกเหนือจากการเปลี่ยนตัวพิมพ์ แต่อาจจำเป็นต้องสร้างกระบวนการใหม่ซึ่งส่งผลต่อประสิทธิภาพ

การนำเสนอไวยากรณ์ `${parameter,,pattern}` ใน Bash 4.0 มีคุณสมบัติในตัวเพื่อแปลงสตริงซึ่งเร็วกว่าและไม่ต้องพึ่งพายูทิลิตีภายนอก มีทางเลือกอื่นใน Bash เอง:

1. `awk`: `echo $str | awk '{print tolower($0)}'`
2. `sed`: `echo $str | sed 's/[A-Z]/\L&/g'`
3. `tr`: `echo $str | tr '[:upper:]' '[:lower:]'` - ตามที่แสดงข้างต้น

ในเรื่องของการใช้งาน, `${parameter,,pattern}` ไม่เพียงแต่เปลี่ยนอักขระ ASCII; มีความตระหนักถึง UTF-8 และสามารถจัดการกับอักขระที่ไม่ใช่ภาษาอังกฤษได้ ทำให้เหมาะสมสำหรับการใช้งานในระดับสากล

## ดูเพิ่มเติม
- Bash Parameter Expansion: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- คำสั่ง `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- การเขียนโปรแกรม AWK: https://www.gnu.org/software/gawk/manual/gawk.html
- ตัวแก้ไขสตรีม `sed`: https://www.gnu.org/software/sed/manual/sed.html
