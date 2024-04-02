---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:25.168311-06:00
description: "Elm \u0E44\u0E21\u0E48\u0E21\u0E35\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\
  \u0E1A\u0E2A\u0E19\u0E38\u0E19 YAML \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E43\u0E19\u0E15\
  \u0E31\u0E27, \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\
  \u0E41\u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E07\u0E25\u0E33\u0E14\
  \u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E21\u0E31\u0E01\
  \u0E43\u0E0A\u0E49\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\
  \u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E48\u0E32\u0E2B\u0E23\u0E37\u0E2D\
  \u0E01\u0E32\u0E23\u0E41\u0E0A\u0E23\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:56.150679-06:00'
model: gpt-4-0125-preview
summary: "Elm \u0E44\u0E21\u0E48\u0E21\u0E35\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\
  \u0E2A\u0E19\u0E38\u0E19 YAML \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E43\u0E19\u0E15\u0E31\
  \u0E27, \u0E0B\u0E36\u0E48\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\
  \u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E07\u0E25\u0E33\u0E14\u0E31\
  \u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E21\u0E31\u0E01\u0E43\
  \u0E0A\u0E49\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\
  \u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E48\u0E32\u0E2B\u0E23\u0E37\u0E2D\u0E01\
  \u0E32\u0E23\u0E41\u0E0A\u0E23\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
weight: 41
---

## อะไรและทำไม?
Elm ไม่มีการสนับสนุน YAML อย่างในตัว, ซึ่งเป็นรูปแบบการเรียงลำดับข้อมูลที่มักใช้สำหรับไฟล์การกำหนดค่าหรือการแชร์ข้อมูล, เนื่องจากมีการเน้นความปลอดภัยของชนิดข้อมูลและผลลัพธ์ที่คาดเดาได้ อย่างไรก็ตาม, โปรแกรมเมอร์บ่อยครั้งที่พบกับ YAML เมื่อต้องจัดการกับ API หรือการกำหนดค่าในการพัฒนาเว็บ, ซึ่งจำเป็นต้องมีวิธีที่เชื่อถือได้ในการแปลงข้อมูล YAML เข้าสู่ระบบชนิดข้อมูลที่เคร่งครัดของ Elm เพื่อการรวมและควบคุมข้อมูลอย่างราบรื่น

## วิธีทำ:
เพื่อจัดการกับ YAML ใน Elm, คุณโดยทั่วไปจำเป็นต้องแปลง YAML เป็น JSON ภายนอกสภาพแวดล้อมของ Elm และจากนั้นใช้ฟังก์ชันการถอดรหัส JSON ที่ฝังอยู่ใน Elm เพื่อทำงานกับข้อมูล แม้ว่าวิธีนี้จะต้องการขั้นตอนการแปลงเพิ่มเติม, แต่ก็ใช้ประโยชน์จากระบบชนิดข้อมูลที่แข็งแกร่งของ Elm เพื่อรับประกันความสมบูรณ์ของข้อมูล โปรแกรมที่ได้รับความนิยมสำหรับการแปลง YAML เป็น JSON รวมถึงเครื่องมือแปลงออนไลน์หรือบริการหลังบ้าน เมื่อคุณมี JSON, คุณสามารถใช้โมดูล `Json.Decode` ของ Elm เพื่อทำงานกับข้อมูล

เริ่มต้น, สมมติว่าคุณมีข้อมูล YAML ดังนี้:

```yaml
person:
  name: Jane Doe
  age: 30
```

แปลงเป็นรูปแบบ JSON:

```json
{
  "person": {
    "name": "Jane Doe",
    "age": 30
  }
}
```

จากนั้น, กำหนดโมเดลและตัวถอดรหัสของคุณใน Elm:

```elm
module Main exposing (..)

import Html exposing (text)
import Json.Decode as Decode

type alias Person =
    { name : String
    , age : Int
    }

personDecoder : Decode.Decoder Person
personDecoder =
    Decode.map2 Person
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

```

เพื่อใช้ตัวถอดรหัสนี้ในการแปลง JSON เป็นชนิดของ Elm:

```elm
import Json.Decode as Decode

jsonString = 
    """
    {
      "person": {
        "name": "Jane Doe",
        "age": 30
      }
    }
    """

decodeResult = Decode.decodeString (Decode.field "person" personDecoder) jsonString

main =
    case decodeResult of
        Ok person ->
            Html.text ("Hello, " ++ person.name ++ "!")
            
        Err _ ->
            Html.text "เกิดข้อผิดพลาดขณะถอดรหัส."
```

ผลลัพธ์ (แสดงในแอปพลิเคชัน Elm):
```
สวัสดี, Jane Doe!
```

วิธีการนี้รับประกันว่าคุณสามารถทำงานกับข้อมูล YAML ใน Elm โดยใช้ JSON เป็นรูปแบบกลาง, ใช้ประโยชน์จากระบบชนิดข้อมูลที่แข็งแกร่งและความสามารถในการถอดรหัส JSON ของ Elm เพื่อจัดการกับข้อมูลภายนอกอย่างปลอดภัยและมีประสิทธิภาพ
