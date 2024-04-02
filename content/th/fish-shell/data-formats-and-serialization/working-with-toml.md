---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:15.507594-06:00
description: "TOML \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u0E04\u0E2D\u0E19\u0E1F\
  \u0E34\u0E01 \u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2D\u0E48\
  \u0E32\u0E19\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E14\u0E22\u0E21\
  \u0E19\u0E38\u0E29\u0E22\u0E4C \u0E41\u0E25\u0E30\u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\
  \u0E2D\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\
  \u0E2B\u0E4C\u0E41\u0E25\u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E42\u0E14\u0E22\u0E40\
  \u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E31\u0E01\u0E23 \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\
  \u0E31\u0E1A TOML\u2026"
lastmod: '2024-03-17T21:57:56.667461-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E44\u0E1F\
  \u0E25\u0E4C\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u0E04\u0E2D\u0E19\u0E1F\u0E34\
  \u0E01 \u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\
  \u0E19\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E14\u0E22\u0E21\u0E19\
  \u0E38\u0E29\u0E22\u0E4C \u0E41\u0E25\u0E30\u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\
  \u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\
  \u0E4C\u0E41\u0E25\u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E42\u0E14\u0E22\u0E40\u0E04\
  \u0E23\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E31\u0E01\u0E23 \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\
  \u0E1A TOML\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## อะไร & ทำไม?
TOML เป็นรูปแบบไฟล์ตั้งค่าคอนฟิก ง่ายต่อการอ่านและเขียนโดยมนุษย์ และง่ายต่อการแยกวิเคราะห์และสร้างโดยเครื่องจักร โปรแกรมเมอร์ทำงานกับ TOML สำหรับไฟล์คอนฟิกที่ชัดเจนและเป็นลำดับชั้นในโปรเจกต์ที่การอ่านเข้าใจเป็นสิ่งสำคัญ

## วิธีการ:
ในการอ่านและจัดการ TOML ใน Fish, คุณอาจใช้เครื่องมือเช่น `yj` ซึ่งสามารถแปลง TOML เป็น JSON นี่คือวิธีการ:

```fish
# ติดตั้ง yj ผ่าน Fisher
fisher install jorgebucaran/yj

# แปลง TOML เป็น JSON
echo 'title = "ตัวอย่าง TOML"' | yj -tj

# ตัวอย่างผลลัพธ์
{"title":"ตัวอย่าง TOML"}
```

ในการเขียน TOML, คุณทำกระบวนการย้อนกลับ:

```fish
# แปลง JSON เป็น TOML
echo '{"title":"ตัวอย่าง JSON"}' | yj -jt

# ตัวอย่างผลลัพธ์
title = "ตัวอย่าง JSON"
```

สำหรับการทำงานที่หนัก, พิจารณาเครื่องมือ CLI สำหรับ TOML เฉพาะเช่น `toml-cli`.

```fish
# ติดตั้ง toml-cli
pip install toml-cli

# ตั้งค่าในไฟล์ TOML
toml set pyproject.toml tool.poetry.version "1.1.4"

# ดึงค่าจากไฟล์ TOML
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## ศึกษาเจาะลึก
TOML (Tom's Obvious, Minimal Language), แนะนำโดย Tom Preston-Werner ในปี 2013, คล้ายคลึงกับ INI แต่มีสเปกระบุและลำดับชั้นข้อมูล JSON และ YAML เป็นทางเลือกหลัก แต่มีข้อเสียของตนเอง: JSON ไม่ง่ายต่อการอ่านโดยมนุษย์ ขณะที่ YAML มีความซับซ้อนมากขึ้น TOML ได้รับการออกแบบมาเพื่อเหมาะสมในสถานการณ์ที่ไฟล์คอนฟิกมักจะถูกบำรุงรักษาด้วยมือ สร้างความสมดุลระหว่างความง่ายและการแสดงออก เมื่อพูดถึงการปรับใช้, มี parser สำหรับ TOML ในภาษาโปรแกรมมิ่งส่วนใหญ่ รวมถึง TomlBombadil สำหรับ Fish ที่สามารถใส่ได้ตรงกับสคริปต์ของคุณ

## ดูเพิ่มเติม
- สเป็คอย่างเป็นทางการของ TOML: https://toml.io
- `yj`, เครื่องมือแปลงระหว่าง TOML, JSON, YAML และ XML: https://github.com/jorgebucaran/yj
- `toml-cli`, ยูทิลิตี้บรรทัดคำสั่งสำหรับ TOML: https://github.com/sdispater/toml-cli
