---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:30.848190-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Elm, \u0E04\
  \u0E38\u0E13\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A XML \u0E42\u0E14\
  \u0E22\u0E43\u0E0A\u0E49\u0E41\u0E1E\u0E47\u0E04\u0E40\u0E01\u0E08 `elm/xml` \u0E19\
  \u0E35\u0E48\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E14\u0E39\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27\u0E43\u0E19\u0E01\u0E32\u0E23\u0E41\
  \u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E2A\u0E48\u0E27\
  \u0E19 XML."
lastmod: '2024-03-17T21:57:56.155425-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Elm, \u0E04\u0E38\u0E13\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\
  \u0E01\u0E31\u0E1A XML \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E41\u0E1E\u0E47\u0E04\
  \u0E40\u0E01\u0E08 `elm/xml` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\
  \u0E14\u0E39\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27\
  \u0E43\u0E19\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E2A\u0E48\u0E27\u0E19 XML."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## วิธีการ:
ใน Elm, คุณจัดการกับ XML โดยใช้แพ็คเกจ `elm/xml` นี่คือการดูอย่างรวดเร็วในการแยกวิเคราะห์ส่วน XML:

```Elm
import Xml.Decode exposing (..)
import Xml.Decode.Pipeline exposing (..)

xmlString = """
<book id="123">
    <title>Elm in Action</title>
    <author>Robin Heggelund Hansen</author>
</book>
"""

type alias Book =
    { id : String
    , title : String
    , author : String
    }

bookDecoder : Decoder Book
bookDecoder =
    decode Book
        |> required "id" (attribute "id")
        |> required "title" (child "title" (content text))
        |> required "author" (child "author" (content text))

case Xml.Decode.fromString bookDecoder xmlString of
    Ok book ->
        -- ทำอะไรกับหนังสือที่ถอดรหัสได้ที่นี่
        Debug.toString book

    Err error ->
        -- ดำเนินการกับข้อผิดพลาด
        Debug.toString error
```

ตัวอย่างผลลัพธ์, สมมติว่าไม่มีข้อผิดพลาด:

```Elm
"{ id = \"123\", title = \"Elm in Action\", author = \"Robin Heggelund Hansen\" }"
```

## ศึกษาเพิ่มเติม
XML (eXtensible Markup Language) มีอยู่มาตั้งแต่ปลายทศวรรษ 90, เมื่อเว็บมีข้อความมากและความต้องการวิธีการที่มีโครงสร้างแต่ยืดหยุ่นในการถ่ายทอดข้อมูลก็เป็นสิ่งสำคัญ. เนื่องจากความยืดยาวและความซับซ้อน, XML ได้สูญเสียพื้นที่บางส่วนให้กับ JSON. อย่างไรก็ตาม, XML ยังคงมีอยู่ในหลายสถานที่, โดยเฉพาะอย่างยิ่งในสภาพแวดล้อมขององค์กรหรือโปรโตคอลเช่น SOAP.

การเข้าใกล้ XML ของ Elm นั้นเป็นแบบฟังก์ชันและปลอดภัยตามประเภทข้อมูล. การใช้แพ็คเกจ `elm/xml` หมายถึงการยอมรับปรัชญาของ Elm ซึ่งเน้นความชัดเจนและความเชื่อถือได้. เมื่อพูดถึงการแยกวิเคราะห์, แพ็คเกจนี้มีตัวถอดรหัส (decoders) หลากหลายที่คุณประกอบกันเพื่อจัดการกับโครงสร้าง XML.

เมื่อเปรียบเทียบกับทางเลือกอื่นๆ เช่น DOMParser ของ JavaScript หรือ ElementTree ของ Python, วิธีของ Elm อาจดูมีคำกล่าวมากขึ้นแต่รับประกันความปลอดภัย. ไม่มีข้อยกเว้นระหว่างการรันโปรแกรมสำหรับฟิลด์ที่หายไปหรือไม่ตรงประเภทข้อมูล; หากมีอะไรผิดพลาด, คุณจะได้รับข้อผิดพลาดในเวลาคอมไพล์.

ฟังก์ชัน decode ใน `elm/xml` อิงหลักการทำงานด้วยการแมปโหนด XML เข้ากับประเภทข้อมูลใน Elm. คุณสร้างตัวถอดรหัสที่สะท้อนรูปแบบข้อมูลของคุณ, รับประกันว่าแอปพลิเคชัน Elm ของคุณจัดการกับ XML ได้อย่างเข้มงวดเช่นเดียวกับโครงสร้างข้อมูลภายในของตัวมันเอง.

การสร้าง XML ใน Elm นั้นน้อยกว่า แต่สามารถทำได้ด้วยตัวเข้าคู่ของ `elm/xml` คือ `Xml.Encode`.

## ดูเพิ่มเติม
- คู่มือ Elm เกี่ยวกับ JSON ซึ่งยังใช้ได้กับความคิดเกี่ยวกับ XML: [https://guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
- มาตรฐาน XML โดย W3C เพื่อการเข้าใจ XML ลึกซึ้งยิ่งขึ้น: [https://www.w3.org/XML/](https://www.w3.org/XML/)
