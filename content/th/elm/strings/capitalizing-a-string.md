---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:38.976397-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\
  \u0E2B\u0E0D\u0E48\u0E41\u0E1A\u0E1A\u0E15\u0E31\u0E27\u0E41\u0E23\u0E01\u0E02\u0E2D\
  \u0E07\u0E04\u0E33\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E40\
  \u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\u0E15\u0E31\u0E27\u0E2D\
  \u0E31\u0E01\u0E29\u0E23\u0E15\u0E31\u0E27\u0E41\u0E23\u0E01\u0E02\u0E2D\u0E07\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E43\u0E2B\
  \u0E49\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\
  \u0E2B\u0E0D\u0E48\u2026"
lastmod: '2024-03-17T21:57:56.111767-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\
  \u0E2B\u0E0D\u0E48\u0E41\u0E1A\u0E1A\u0E15\u0E31\u0E27\u0E41\u0E23\u0E01\u0E02\u0E2D\
  \u0E07\u0E04\u0E33\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E40\
  \u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\u0E15\u0E31\u0E27\u0E2D\
  \u0E31\u0E01\u0E29\u0E23\u0E15\u0E31\u0E27\u0E41\u0E23\u0E01\u0E02\u0E2D\u0E07\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E43\u0E2B\
  \u0E49\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E43\
  \u0E2B\u0E0D\u0E48\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E15\u0E31\u0E27\u0E2D\u0E31\
  \u0E01\u0E29\u0E23\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E43\u0E2B\u0E0D\u0E48\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การทำให้สตริงเป็นตัวพิมพ์ใหญ่แบบตัวแรกของคำหมายถึงการเปลี่ยนแปลงตัวอักษรตัวแรกของสตริงที่กำหนดให้เป็นตัวพิมพ์ใหญ่ ขณะที่เก็บส่วนที่เหลือไว้เป็นตัวพิมพ์เล็ก มักจะใช้เพื่อการจัดรูปแบบหรือการอ่านที่เป็นมาตรฐาน โปรแกรมเมอร์บ่อยครั้งที่ทำงานนี้เพื่อให้แน่ใจว่าข้อมูลถูกนำเสนออย่างสอดคล้องกัน เฉพาะอย่างยิ่งในส่วนติดต่อผู้ใช้หรือเมื่อประมวลผลและแสดงข้อมูลที่ผู้ใช้ป้อน

## วิธีการ:

ใน Elm ไม่มีฟังก์ชันที่สร้างขึ้นมาเป็นพิเศษสำหรับการทำให้สตริงเป็นตัวพิมพ์ใหญ่ อย่างไรก็ตาม คุณสามารถทำได้ง่ายๆ โดยใช้ฟังก์ชันในโมดูล `String` ที่มีอยู่แล้ว เช่น `toUpper`, `toLower`, `left`, และ `dropLeft`

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- ตัวอย่างการใช้งาน
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- ผลลัพธ์: "Hello World"
```

สำหรับสถานการณ์ที่ซับซ้อนมากขึ้นหรือหากคุณต้องการใช้ไลบรารีที่มีวิธีโดยตรงในการทำให้สตริงเป็นตัวพิมพ์ใหญ่ เมื่อมีการพิจารณาแพคเกจของบุคคลที่สาม เช่น `elm-community/string-extra` อย่างไรก็ตาม ตามการอัปเดตครั้งล่าสุดของฉัน ระบบนิเวศของ Elm สนับสนุนให้จัดการกับงานเช่นนี้โดยใช้ฟังก์ชันที่มีอยู่เพื่อให้ภาษาและโปรเจกต์เรียบง่าย

```elm
import String.Extra as StringExtra

-- ในกรณีที่มีฟังก์ชัน `capitalize` ในไลบรารีของบุคคลที่สาม
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- ตัวอย่างการใช้งานกับฟังก์ชันจากไลบรารีที่เป็นสมมติฐาน
main =
    "this is elm" |> capitalizeWithLibrary
    -- ผลลัพธ์ที่เป็นสมมติฐาน: "This is elm"
```

ตรวจสอบในที่เก็บแพคเกจของ Elm สำหรับไลบรารีที่ล่าสุดและได้รับการต้อนรับมากที่สุดสำหรับการจัดการสตริงหากคุณกำลังมองหาความสามารถเพิ่มเติมเกินกว่าคลังมาตรฐาน
