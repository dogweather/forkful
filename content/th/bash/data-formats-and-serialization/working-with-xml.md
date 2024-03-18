---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:04.411439-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C, \u0E01\u0E32\u0E23\u0E2A\
  \u0E01\u0E31\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E41\u0E25\u0E30\u0E01\
  \u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E2D\u0E07 Extensible Markup\
  \ Language \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E0B\u0E2D\u0E1F\u0E15\
  \u0E4C\u0E41\u0E27\u0E23\u0E4C\u0E21\u0E31\u0E01\u0E08\u0E30\u0E21\u0E35\u0E1B\u0E31\
  \u0E0D\u0E2B\u0E32\u0E01\u0E31\u0E1A XML\u2026"
lastmod: '2024-03-17T21:57:56.421546-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\
  \u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C, \u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\
  \u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\
  \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E43\u0E19\
  \u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E2D\u0E07 Extensible Markup Language\
  \ \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E0B\u0E2D\u0E1F\u0E15\u0E4C\
  \u0E41\u0E27\u0E23\u0E4C\u0E21\u0E31\u0E01\u0E08\u0E30\u0E21\u0E35\u0E1B\u0E31\u0E0D\
  \u0E2B\u0E32\u0E01\u0E31\u0E1A XML\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
---

{{< edit_this_page >}}

## คืออะไรและทำไม?
การทำงานกับ XML หมายถึงการแยกวิเคราะห์, การสกัดข้อมูล, และการจัดการข้อมูลในรูปแบบของ Extensible Markup Language นักพัฒนาซอฟต์แวร์มักจะมีปัญหากับ XML เพราะเป็นรูปแบบการแลกเปลี่ยนข้อมูลที่แพร่หลายสำหรับการตั้งค่า, API และอื่น ๆ

## วิธีการ:
นี่คือวิธีการแยก XML ใน Bash โดยใช้เครื่องมืออะไร? xmllint และ xmlstarlet การวนซ้ำผ่านองค์ประกอบ XML? แน่นอน เป็นตัวอย่างพร้อมผลลัพธ์:

```bash
# สมมติว่าได้ติดตั้ง xmlstarlet แล้ว
# ติดตั้งโดยใช้: apt-get install xmlstarlet

# การแยก XML 
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# สกัดชื่อด้วย xmlstarlet
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# ผลลัพธ์ควรจะเป็น:
# Apple
# Banana
```

## ตรวจสอบอย่างละเอียด
ในช่วงปี 1990, XML ได้รับความนิยมเป็นทางเลือกที่ง่ายกว่า SGML แต่มีโครงสร้างมากกว่า HTML ตอนนี้, มันมีคู่แข่ง – เช่น JSON, YAML แต่ XML ยังคงเป็นที่นิยม, โดยเฉพาะในการตั้งค่าและบริการเว็บที่ใช้ SOAP

ในด้านเครื่องมือ, xmllint สะดวกสบายสำหรับการตรวจสอบ XML, คำสั่ง xpath xmlstarlet เป็นมีดสวิสสำหรับเรื่องราว XML – คำถาม, แก้ไข, ตรวจสอบ, แปลง ในสคริปต์ bash, พวกมันเป็นฮีโร่สำหรับงาน XML

ภายใต้ฝา, xmllint ใช้ libxml2 – พาร์เซอร์ XML ภาษา C มันเร็ว, แต่ข้อผิดพลาด? ยากต่อการตีความ และ xmlstarlet? แม่แบบแบบวนซ้ำและการสนับสนุน EXSLT ทำให้ปวดหัว, แต่ทรงพลัง

## ดูเพิ่มเติม
- [xmlsoft.org](http://xmlsoft.org/): สิ่งของ Libxml2 และ xmllint
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): ปัญหาและการแก้ไขในโลกจริง
- [W3Schools XML Tutorial](https://www.w3schools.com/xml/): พื้นฐานของ XML
