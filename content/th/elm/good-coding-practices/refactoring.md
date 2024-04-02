---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:57.650586-06:00
description: "\u0E01\u0E32\u0E23 Refactor \u0E19\u0E31\u0E49\u0E19\u0E40\u0E2B\u0E21\
  \u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E04\u0E27\u0E32\
  \u0E21\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u0E10\u0E32\
  \u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u2014\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\
  \u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E0B\u0E49\u0E33\u0E02\
  \u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\
  \u0E48\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\
  \u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\
  \u2026"
lastmod: '2024-03-17T21:57:56.137395-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23 Refactor \u0E19\u0E31\u0E49\u0E19\u0E40\u0E2B\u0E21\u0E37\
  \u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E04\u0E27\u0E32\u0E21\
  \u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u0E10\u0E32\u0E19\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u2014 \u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\
  \u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E0B\u0E49\u0E33\u0E02\u0E2D\u0E07\
  \u0E42\u0E04\u0E49\u0E14\u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E42\
  \u0E14\u0E22\u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E1E\u0E24\
  \u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\u2026"
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
weight: 19
---

## อะไร & ทำไม?
การ Refactor นั้นเหมือนกับการทำความสะอาดใหญ่ในฐานข้อมูลของคุณ— หมายถึงการปรับโครงสร้างซ้ำของโค้ดที่มีอยู่โดยไม่เปลี่ยนพฤติกรรมภายนอก โปรแกรมเมอร์ทำเช่นนี้เพื่อให้โค้ดสามารถอ่านได้ง่ายขึ้น ลดความซับซ้อน ปรับปรุงความสามารถในการบำรุงรักษา และทำให้ขยายงานได้ง่ายขึ้น

## วิธีการ:
พิจารณาว่าคุณมีฟังก์ชัน Elm ที่ทำงานมากเกินไป เช่น ผสมผสานโลจิก UI กับการอัปเดตสถานะ เป็นตัวอย่างที่สมบูรณ์สำหรับการ Refactor ตัวอย่างเดิม:

```Elm
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    let
        updatedModel = { model | userInput = input }
    in
    if String.length input > 5 then
        ( updatedModel, Cmd.none )
    else
        ( model, Cmd.none )
```

หลังจาก Refactor เราแยกเรื่องราวโดยนำโลจิกออกมาเป็นฟังก์ชันแยกต่างหาก:

```Elm
-- โลจิกการอัปเดตคืออิสระ
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- โลจิกการจัดรูปแบบ (view) ก็แยกต่างหาก
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- ล้างข้อมูลป้อนหากสั้นเกินไป เป็นตัวอย่างกฎ

-- ฟังก์ชั่นการอัปเดตตอนนี้ใช้ฟังก์ชั่นช่วยเหลือ
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
ด้วยการเปลี่ยนแปลงเหล่านี้ คุณมีการแยกความกังวลอย่างชัดเจน และแต่ละฟังก์ชั่นสามารถเข้าใจและทดสอบได้ง่ายขึ้น

## การศึกษาลึก
การ Refactor ตามรูปแบบเป็นการปฏิบัติที่สามารถติดตามไปยังยุคแรกๆ ของการเขียนโปรแกรมเมื่อต้นทุนของการเปลี่ยนแปลงโค้ดกำลังถูกจำเป็นเป็นส่วนหนึ่งของกระบวนการพัฒนา โดยเฉพาะหนังสือ "Refactoring: Improving the Design of Existing Code" ของ Martin Fowler ที่เผยแพร่ในช่วงปลาย 1990s จริงๆ แล้วได้ตั้งเวทีสำหรับการ Refactor ด้วยวิธีการที่มีโครงสร้างและแคตตาล็อกของ "code smells" เพื่อระบุโอกาสในการ Refactor

ในบริบทของ Elm, Refactor นั้นเชื่อมโยงกับจุดแข็งของภาษา เช่น ระบบการพิมพ์ที่แข็งแกร่ง ซึ่งส่งเสริมความมั่นใจในระหว่างกระบวนการ ทางเลือกสำหรับการ Refactor ด้วยตนเองอาจรวมถึงเครื่องมือการเปลี่ยนรูปแบบโค้ดแบบอัตโนมัติ แต่เครื่องมือด้านนี้ของ Elm ยังคงอยู่ในระหว่างการพัฒนาเมื่อเปรียบเทียบกับภาษาเก่า รายละเอียดการดำเนินการมักจะเกี่ยวข้องกับการ Refactor ทั่วไป เช่น การสกัดฟังก์ชั่น การเปลี่ยนชื่อ และการทำให้เงื่อนไขเรียบง่ายขึ้น ตัวคอมไพเลอร์ของ Elm เป็นพันธมิตรหลักในการ Refactor เนื่องจากมันจะไม่ยอมให้คุณผิดพลาดมาก—มันเตือนเสมอเมื่อมีสิ่งใดไม่ถูกต้อง รับประกันว่าโค้ดที่คุณ Refactor ยังคงทำงาน

## ดูเพิ่มเติม
- ["Refactoring: Improving the Design of Existing Code" โดย Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [Elm Discourse - หัวข้อเกี่ยวกับ Refactoring](https://discourse.elm-lang.org/search?q=refactoring)
