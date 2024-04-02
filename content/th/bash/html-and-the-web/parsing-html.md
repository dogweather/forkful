---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:50.261336-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19 HTML \u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\
  \u0E23\u0E0A\u0E33\u0E41\u0E2B\u0E25\u0E30\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\
  \u0E32\u0E07\u0E41\u0E25\u0E30\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E02\u0E2D\
  \u0E07\u0E44\u0E1F\u0E25\u0E4C HTML \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E36\u0E07\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E2D\u0E01\u0E21\u0E32 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\u0E32\u0E16\
  \u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E14\u0E31\u0E14\u0E41\u0E1B\
  \u0E25\u0E07\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32,\u2026"
lastmod: '2024-03-17T21:57:56.395290-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19 HTML \u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\
  \u0E23\u0E0A\u0E33\u0E41\u0E2B\u0E25\u0E30\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\
  \u0E32\u0E07\u0E41\u0E25\u0E30\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E02\u0E2D\
  \u0E07\u0E44\u0E1F\u0E25\u0E4C HTML \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E36\u0E07\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E2D\u0E01\u0E21\u0E32 \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\
  \u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\u0E32\u0E16\
  \u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E14\u0E31\u0E14\u0E41\u0E1B\
  \u0E25\u0E07\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32,\u2026"
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
weight: 43
---

## อะไร & ทำไม?

การแยกส่วน HTML หมายถึงกระบวนการชำแหละโครงสร้างและเนื้อหาของไฟล์ HTML เพื่อดึงข้อมูลออกมา โปรแกรมเมอร์ทำเช่นนี้เพื่อเข้าถึงข้อมูล, ดัดแปลงเนื้อหา, หรือเก็บข้อมูลจากเว็บไซต์

## วิธีการ:

Bash ไม่ใช่ตัวเลือกแรกสำหรับการแยกส่วน HTML, แต่สามารถทำได้โดยใช้เครื่องมืออย่าง `grep`, `awk`, `sed`, หรือยูทิลิตีภายนอกอย่าง `lynx` สำหรับความเข้มแข็ง, เราจะใช้ `xmllint` จากแพ็คเกจ `libxml2`

```bash
# ติดตั้ง xmllint ถ้าต้องการ
sudo apt-get install libxml2-utils

# ตัวอย่าง HTML
cat > sample.html <<EOF
<html>
<head>
  <title>ตัวอย่างหน้า</title>
</head>
<body>
  <h1>สวัสดี, Bash!</h1>
  <p id="myPara">Bash สามารถอ่านฉันได้.</p>
</body>
</html>
EOF

# แยกส่วนหัวข้อ
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "หัวข้อคือ: $title"

# ดึงเนื้อหาย่อหน้าโดย ID
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "เนื้อหาย่อหน้าคือ: $para"
```

ผลลัพธ์:
```
หัวข้อคือ: ตัวอย่างหน้า
เนื้อหาย่อหน้าคือ: Bash สามารถอ่านฉันได้.
```

## วิเคราะห์ลึก

ในอดีต, โปรแกรมเมอร์ใช้เครื่องมือที่ใช้ regex อย่าง `grep` เพื่อสแกน HTML, แต่วิธีนี้ค่อนข้างยุ่งยาก HTML ไม่ใช่ภาษาที่ตายตัว—มันเป็นภาษาตามบริบท เครื่องมือดั้งเดิมพลาดเรื่องนี้และอาจมีความผิดพลาดได้

มีทางเลือกอื่นหรือ? มากมาย เช่น Python กับ Beautiful Soup, PHP กับ DOMDocument, JavaScript กับ DOM parsers—ภาษาเหล่านี้มีไลบรารีที่ออกแบบมาเพื่อเข้าใจโครงสร้างของ HTML

การใช้ `xmllint` ในสคริปต์ bash เหมาะสำหรับงานง่ายๆ มันเข้าใจ XML, และโดยการขยาย, XHTML แต่ HTML ปกติอาจไม่คาดเดาได้ มันไม่เสมอไปตามกฎเคร่งครัดของ XML `xmllint` บังคับให้ HTML เข้าไปอยู่ในโมเดล XML ซึ่งทำงานได้ดีกับ HTML ที่มีโครงสร้างดี แต่อาจมีปัญหากับ HTML ที่ไม่เป็นระเบียบ

## ดูเพิ่มเติม

- [W3Schools - HTML DOM Parser](https://www.w3schools.com/xml/dom_intro.asp): ช่วยให้เข้าใจ HTML DOM
- [MDN Web Docs - Parsing and serializing XML](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): สำหรับหลักการแยกส่วน XML ที่ใช้กับ XHTML
- [Beautiful Soup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): ไลบรารีของ Python สำหรับการแยกส่วน HTML
- [libxml2 Documentation](http://xmlsoft.org/): รายละเอียดเกี่ยวกับ `xmllint` และเครื่องมือ XML ที่เกี่ยวข้อง
