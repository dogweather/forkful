---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:16.384885-06:00
description: ''
lastmod: '2024-04-05T22:51:14.527864-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

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
