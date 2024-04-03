---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:50.584851-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E42\u0E2D\u0E40\u0E04\
  , \u0E16\u0E36\u0E07\u0E40\u0E27\u0E25\u0E32\u0E42\u0E04\u0E49\u0E14\u0E41\u0E25\
  \u0E49\u0E27 Elm \u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\u0E39\u0E25 `Http` \u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E19\u0E35\u0E48\
  \u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E14\u0E36\u0E07 JSON \u0E1A\u0E32\u0E07\u0E2A\u0E48\u0E27\u0E19."
lastmod: '2024-03-17T21:57:56.125308-06:00'
model: gpt-4-0125-preview
summary: "\u0E42\u0E2D\u0E40\u0E04, \u0E16\u0E36\u0E07\u0E40\u0E27\u0E25\u0E32\u0E42\
  \u0E04\u0E49\u0E14\u0E41\u0E25\u0E49\u0E27 Elm \u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\
  \u0E39\u0E25 `Http` \u0E43\u0E19\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\
  \u0E02\u0E2D HTTP \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\
  \u0E48\u0E32\u0E07\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E36\u0E07 JSON \u0E1A\u0E32\
  \u0E07\u0E2A\u0E48\u0E27\u0E19."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## วิธีการ:
โอเค, ถึงเวลาโค้ดแล้ว Elm ใช้โมดูล `Http` ในการส่งคำขอ HTTP นี่คือตัวอย่างเพื่อดึง JSON บางส่วน:

```Elm
import Http
import Json.Decode as Decode

type alias User =
    { id : Int
    , username : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "username" Decode.string)

fetchUser : Cmd Msg
fetchUser =
    Http.get
        { url = "https://api.example.com/user/1"
        , decoder = userDecoder
        }
        |> Http.send UserFetched

type Msg
    = UserFetched (Result Http.Error User)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UserFetched (Ok user) ->
            ({ model | user = Just user }, Cmd.none)

        UserFetched (Err _) ->
            (model, Cmd.none)
```

ตัวอย่างข้อมูลเมื่อ `UserFetched` เป็น `Ok user`:

```Elm
{ id = 1, username = "ElmerFudd" }
```

## ดำดิ่งลึก
การส่งคำขอ HTTP ไม่ใช่เรื่องใหม่; มันเป็นหลักสูตรหลังการสื่อสารเว็บตั้งแต่ยุค 90's Elm ห่อความซับซ้อนนี้ไว้ในโมดูล `Http` ที่ใช้งานง่าย, โฟกัสที่ความปลอดภัยและความง่ายดาย ไม่เหมือนกับยุคแรกๆ, Elm แยกส่วนที่ยุ่งยากเช่น XMLHttprequest และการแปลง JSON Alternatives เช่นการใช้ Fetch API ของ JavaScript หรือ XMLHttpRequest โดยตรงเป็นไปได้ด้วย ports, แต่วิธีที่มีอยู่ใน Elm รักษาโค้ดของคุณให้ปลอดภัยและบริสุทธิ์ มันจัดการกับผลข้างเคียงผ่านสถาปัตยกรรมที่ทรงพลังโดยไม่ทำลายความน่าเชื่อถือของแอปพลิเคชันของคุณ

## ดูเพิ่มเติม
สำหรับคำอธิบายและการแก้ไขปัญหาที่ละเอียดยิ่งขึ้น ดูที่ทรัพยากรเหล่านี้:

- เอกสารประกอบของ Elm สำหรับ HTTP: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- การ Decode JSON ใน Elm: [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
- คู่มือของ Elm ว่าด้วยคำขอ HTTP: [https://guide.elm-lang.org/effects/http.html](https://guide.elm-lang.org/effects/http.html)
- Elm Discuss สำหรับข้อมูลเชิงลึกจากชุมชน: [https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)
