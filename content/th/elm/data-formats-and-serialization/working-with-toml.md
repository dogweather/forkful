---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:04.560530-06:00
description: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious,\
  \ Minimal Language \u0E40\u0E1B\u0E47\u0E19\u0E20\u0E32\u0E29\u0E32\u0E2D\u0E19\u0E38\
  \u0E01\u0E23\u0E21\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C Elm \u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\
  \u0E43\u0E19\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u2026"
lastmod: '2024-03-17T21:57:56.153916-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0E22\u0E48\u0E2D\u0E21\u0E32\u0E08\u0E32\u0E01 Tom's Obvious, Minimal\
  \ Language \u0E40\u0E1B\u0E47\u0E19\u0E20\u0E32\u0E29\u0E32\u0E2D\u0E19\u0E38\u0E01\
  \u0E23\u0E21\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C Elm \u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E43\
  \u0E19\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32 \u0E40\u0E1E\
  \u0E23\u0E32\u0E30\u0E21\u0E31\u0E19\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E2D\u0E48\
  \u0E32\u0E19\u0E42\u0E14\u0E22\u0E04\u0E19\u0E44\u0E14\u0E49\u0E07\u0E48\u0E32\u0E22\
  \u0E41\u0E25\u0E30\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E41\u0E1C\u0E19\
  \u0E17\u0E35\u0E48\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E04\u0E39\u0E48\u0E04\u0E35\u0E22\
  \u0E4C-\u0E04\u0E48\u0E32\u0E17\u0E35\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\
  \u0E43\u0E19\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\u0E44\
  \u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E23\u0E35\u0E22\u0E1A\u0E23\u0E49\
  \u0E2D\u0E22."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\
  \u0E31\u0E1A TOML"
weight: 39
---

## วิธีการ:
Elm ไม่มีตัวแยกสำเร็จรูป TOML ในตัว แต่คุณสามารถทำงานร่วมกับ JavaScript หรือใช้แพคเกจของชุมชน นี่คือวิธีที่คุณอาจจะวิเคราะห์ TOML โดยใช้แพคเกจ `elm-toml` ที่เป็นสมมุติ:

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

สำหรับการถอดรหัสค่าเฉพาะ:

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

ตัวอย่างผลลัพธ์สำหรับ `port` อาจเป็น `Ok 8080` ถ้าการถอดรหัสสำเร็จ

## ดำดิ่งลึก
TOML ถูกสร้างขึ้นโดย Tom Preston-Werner ผู้ร่วมก่อตั้ง GitHub เป็นภาษาง่าย ๆ สำหรับไฟล์การตั้งค่า มันแข่งขันกับ YAML และ JSON; ไวยากรณ์ของ TOML มุ่งหวังสำหรับสิ่งที่ดีที่สุดของทั้งสองโลกด้วยความสนใจในการทำให้ง่ายสำหรับมนุษย์ในการอ่านและเขียน

ใน Elm, เพื่อจัดการกับ TOML, คุณโดยปกติต้องผ่าน JavaScript interop ซึ่งอาจดูเป็นอุปสรรคอย่างหนึ่งได้ โชคดีที่ชุมชน Elm อุดมไปด้วยทรัพยากร และมีแพคเกจภายนอกหลายตัว เช่น แพคเกจ `elm-toml` ที่สมมุติขึ้น น่าจะใช้ `Port` ของ Elm เพื่อสื่อสารกับตัวแยก TOML ของ JavaScript หรือประมวลผลการแยกโดยตรงใน Elm

อุปสรรคหลักใน Elm คือมันประเภทแบบสถิติทุกอย่าง ดังนั้นคุณจะต้องเขียนตัวถอดรหัสแบบกำหนดเองเพื่อจัดการกับโครงสร้างข้อมูลที่ต่างกันภายใน TOML ซึ่งอาจดูเป็นไปโดยพูดมากหน่อยแต่เพิ่มความปลอดภัย

## ดูเพิ่มเติม
สำหรับข้อกำหนดและข้อมูลเพิ่มเติมเกี่ยวกับ TOML ให้ตรวจสอบ [TOML](https://toml.io)
หากคุณกำลังค้นหาวิธีการเข้ามือทำกับ Elm และ JavaScript interop ให้เริ่มต้นด้วยคู่มืออย่างเป็นทางการ: [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
สำหรับแพคเกจของชุมชนหรือการมีส่วนร่วม ให้เรียกดู [Elm Packages](https://package.elm-lang.org/)
