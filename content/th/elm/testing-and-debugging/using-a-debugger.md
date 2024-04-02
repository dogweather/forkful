---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:10.447374-06:00
description: "Elm \u0E44\u0E21\u0E48\u0E21\u0E35\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\
  \u0E01\u0E2D\u0E23\u0E4C\u0E43\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E17\u0E35\u0E48\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E40\u0E04\u0E22\u0E40\u0E2B\
  \u0E47\u0E19\u0E43\u0E19\u0E20\u0E32\u0E29\u0E32\u0E2D\u0E37\u0E48\u0E19\u0E46 \u0E40\
  \u0E0A\u0E48\u0E19 JavaScript \u0E1A\u0E19\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\
  \u0E21\u0E37\u0E2D\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E40\u0E1A\u0E23\u0E32\u0E27\u0E4C\
  \u0E40\u0E0B\u0E2D\u0E23\u0E4C \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E01\u0E47\
  \u0E15\u0E32\u0E21 \u0E0A\u0E38\u0E21\u0E0A\u0E19 Elm\u2026"
lastmod: '2024-03-17T21:57:56.133581-06:00'
model: gpt-4-0125-preview
summary: "Elm \u0E44\u0E21\u0E48\u0E21\u0E35\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\
  \u0E2D\u0E23\u0E4C\u0E43\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E17\
  \u0E35\u0E48\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E40\u0E04\u0E22\u0E40\u0E2B\u0E47\
  \u0E19\u0E43\u0E19\u0E20\u0E32\u0E29\u0E32\u0E2D\u0E37\u0E48\u0E19\u0E46 \u0E40\u0E0A\
  \u0E48\u0E19 JavaScript \u0E1A\u0E19\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\
  \u0E37\u0E2D\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E40\u0E1A\u0E23\u0E32\u0E27\u0E4C\u0E40\
  \u0E0B\u0E2D\u0E23\u0E4C \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E01\u0E47\u0E15\
  \u0E32\u0E21 \u0E0A\u0E38\u0E21\u0E0A\u0E19 Elm\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
weight: 35
---

## วิธีการ:
Elm ไม่มีดีบักเกอร์ในตัวอย่างที่ผู้ใช้เคยเห็นในภาษาอื่นๆ เช่น JavaScript บนเครื่องมือพัฒนาเบราว์เซอร์ อย่างไรก็ตาม ชุมชน Elm ได้พัฒนาเครื่องมือเพื่อเติมเต็มช่องว่างนี้ นี่คือวิธีที่คุณสามารถใช้ `elm-debug-transformer` เพื่อดีบักร์แอป Elm ของคุณ:

```Elm
-- ติดตั้ง elm-debug-transformer (แพ็คเกจของ Node)

1. npm install -g elm-debug-transformer

-- ใช้ elm-debug-transformer เพื่อเริ่มแอปของคุณ

2. elm-debug-transformer --port=8000 yourMainElmFile.elm 
```

เมื่อ `elm-debug-transformer` เริ่มทำงาน, มันจะสร้างการเชื่อมต่อ WebSocket สำหรับการบันทึกข้อมูล คุณจะเห็นข้อมูลดีบักในคอนโซลของเบราว์เซอร์ของคุณ เพื่อตรวจสอบโครงสร้างข้อมูลของโปรแกรมในจุดต่างๆ ของแอปพลิเคชั่นของคุณ

ใน Elm 0.19 และใหม่กว่า, ฟังก์ชันในโมดูล `Debug` เช่น `Debug.log` และ `Debug.todo` สามารถช่วยคุณติดตามค่าและทำเครื่องหมายบางส่วนของโค้ดที่ยังทำไม่เสร็จได้อย่างตั้งใจ นี่คือวิธีการใช้ Debug.log:

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Incrementing" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Decrementing" { model | count = model.count - 1 }, Cmd.none )
```

คุณจะเห็นข้อความ "Incrementing" หรือ "Decrementing" ในคอนโซลของเบราว์เซอร์คุณพร้อมกับสถานะใหม่ของ `model`

## การศึกษาลึก
Evan Czaplicki ผู้สร้าง Elm มีเป้าหมายที่จะทำให้ภาษาที่มีข้อบกพร่องในปกติที่เกิดขึ้นเป็นไปไม่ได้หรือง่ายต่อการจับได้ นี่คือเหตุผลว่าทำไมหลักของ Elm ไม่รวมฟังก์ชันดีบักแบบดั้งเดิม การวิเคราะห์แบบสถิติและการอนุมานประเภทของ Elm มีส่วนช่วยลดข้อผิดพลาดเวลาทำงานอย่างมาก ซึ่งลดความจำเป็นในการดีบักเวลาทำงานที่ซับซ้อน ทางเลือกในอดีตรวมถึงการใช้`elm-reactor` ที่ให้ฟีเจอร์การดีบักแบบย้อนเวลา—วิธีในการย้อนกลับและเล่นซ้ำการกระทำในแอปของคุณซึ่งตอนนี้ไม่ควรใช้แล้ว

ปัจจุบัน เครื่องมือเช่น `elm-debug-transformer` และการใช้โมดูล `Debug` ของ Elm ช่วยเติมเต็มช่องว่าง ในขณะที่โมดูล `Debug` มีจุดประสงค์สำหรับการใช้งานในระหว่างการพัฒนาเท่านั้นและควรถูกลบออกก่อนการสร้างขั้นตอนการผลิต มันเป็นเครื่องมือที่มีค่ามากสำหรับการระบุและบันทึกการเปลี่ยนแปลงสถานะ

จำไว้ว่าเทคนิคการดีบักของ JavaScript แบบดั้งเดิม เช่น จุดหยุดหรือการดำเนินการทีละขั้นตอน ไม่สามารถใช้ได้โดยตรงใน Elm เนื่องจากโครงสร้างและ Elm runtime การจัดการอัพเดทสถานะ Elm แนะนำให้คุณโครงสร้างโปรแกรมให้อยู่ในลักษณะที่การไหลข้อมูลเป็นไปอย่างชัดเจน ทำตามประเภทข้อมูลและรับประกันมิวเทเบิลที่ชัดเจน ลดกรณีที่จำเป็นต้องดีบัก

## ดูเพิ่มเติม
- คู่มืออย่างเป็นทางการของ Elm เกี่ยวกับการจัดการข้อยกเว้นเวลาทำงาน: https://guide.elm-lang.org/error_handling/
- ที่เก็บ GitHub ของ `elm-debug-transformer`: https://github.com/kraklin/elm-debug-transformer
- กระทู้ Elm discourse ที่อภิปรายเกี่ยวกับกลยุทธ์การดีบัก: https://discourse.elm-lang.org/c/show-and-tell/debugging
- เอกสารประกอบโมดูล `Debug` ของ Elm: https://package.elm-lang.org/packages/elm/core/latest/Debug
