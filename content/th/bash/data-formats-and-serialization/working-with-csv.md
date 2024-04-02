---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:16.384885-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV (Comma-Separated Values) \u0E43\u0E19 Bash \u0E04\u0E37\u0E2D\
  \u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E41\u0E25\u0E30\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E41\u0E1A\
  \u0E1A\u0E15\u0E32\u0E23\u0E32\u0E07\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\u0E1A\u0E43\
  \u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\
  \u0E18\u0E23\u0E23\u0E21\u0E14\u0E32 (plain text)\u2026"
lastmod: '2024-03-17T21:57:56.419719-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV (Comma-Separated Values) \u0E43\u0E19 Bash \u0E04\u0E37\u0E2D\
  \u0E01\u0E32\u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E41\u0E25\u0E30\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E41\u0E1A\
  \u0E1A\u0E15\u0E32\u0E23\u0E32\u0E07\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\u0E1A\u0E43\
  \u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\
  \u0E18\u0E23\u0E23\u0E21\u0E14\u0E32 (plain text)\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## อะไร & ทำไม?
การทำงานกับไฟล์ CSV (Comma-Separated Values) ใน Bash คือการประมวลผลและจัดการข้อมูลแบบตารางที่เก็บในรูปแบบข้อความธรรมดา (plain text) นี่เป็นสิ่งสำคัญสำหรับโปรแกรมเมอร์เนื่องจากช่วยให้สามารถทำงานอัตโนมัติเกี่ยวกับการแปลงข้อมูล การวิเคราะห์ และการรวมข้อมูลโดยตรงจาก command line โดยไม่ต้องใช้เครื่องมือหรือสภาพแวดล้อมการพัฒนาที่หนักหน่วงกว่า

## วิธีทำ:

**อ่านไฟล์ CSV ทีละบรรทัด**

```bash
while IFS=, read -r column1 column2 column3
do
  echo "Column 1: $column1, Column 2: $column2, Column 3: $column3"
done < sample.csv
```

*ตัวอย่างผลลัพธ์:*

```
Column 1: id, Column 2: name, Column 3: email
...
```

**กรองแถว CSV ตามเงื่อนไข**

โดยการใช้ `awk`, คุณสามารถกรองแถวได้อย่างง่ายดาย เช่น เพื่อหาแถวที่คอลัมน์ที่สองเท่ากับ "Alice":

```bash
awk -F, '$2 == "Alice" { print $0 }' sample.csv
```

**แก้ไขค่าของคอลัมน์**

เพื่อเปลี่ยนคอลัมน์ที่สองเป็นตัวพิมพ์ใหญ่:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' sample.csv
```

**เรียงลำดับไฟล์ CSV ตามคอลัมน์**

คุณสามารถเรียงลำดับไฟล์ CSV ตามคอลัมน์ที่สาม (ตามตัวเลข):

```bash
sort -t, -k3,3n sample.csv
```

**ใช้ `csvkit` สำหรับงานที่ซับซ้อนกว่า**

`csvkit` เป็นชุดเครื่องมือที่ใช้แบบ command-line ในการแปลงไปและทำงานกับ CSV สามารถติดตั้งได้ผ่าน pip

เพื่อแปลงไฟล์ JSON เป็น CSV:

```bash
in2csv data.json > data.csv
```

เพื่อค้นไฟล์ CSV โดยใช้ SQL:

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" sample.csv
```

*หมายเหตุ: การติดตั้ง `csvkit` ต้องใช้ Python และสามารถทำได้โดยการใช้ `pip install csvkit`.*
