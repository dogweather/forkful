---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:40.055259-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E08\u0E32\u0E01\u0E2D\u0E34\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E40\
  \u0E19\u0E47\u0E15\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u0E40\u0E02\u0E49\u0E32\u0E2A\
  \u0E39\u0E48\u0E41\u0E2D\u0E1B\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E41\u0E2A\u0E14\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E1B\u0E23\u0E30\u0E21\
  \u0E27\u0E25\u0E1C\u0E25\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E19\u0E31\u0E49\u0E19\
  \ \u0E46\u2026"
lastmod: '2024-03-17T21:57:56.127269-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E08\u0E32\u0E01\u0E2D\u0E34\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E40\
  \u0E19\u0E47\u0E15\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07\u0E40\u0E02\u0E49\u0E32\u0E2A\
  \u0E39\u0E48\u0E41\u0E2D\u0E1B\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E41\u0E2A\u0E14\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E1B\u0E23\u0E30\u0E21\
  \u0E27\u0E25\u0E1C\u0E25\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E19\u0E31\u0E49\u0E19\
  \ \u0E46\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การดาวน์โหลดเว็บเพจหมายถึงการเข้าถึงข้อมูลจากอินเทอร์เน็ตโดยตรงเข้าสู่แอปของคุณเพื่อแสดงหรือประมวลผลข้อมูลนั้น ๆ โปรแกรมเมอร์ทำเช่นนี้เพื่อเข้าถึงข้อมูลแบบเรียลไทม์หรือเพื่อให้เนื้อหาแบบไดนามิกแก่ผู้ใช้

## วิธีการ:

Elm กำหนดให้ผลกระทบด้านข้างเช่น คำขอ HTTP ต้องถูกจัดโครงสร้างเป็นคำสั่ง คุณจะใช้โมดูล `Http` เพื่อดึงและจัดการกับการตอบสนอง

```Elm

module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Http

type alias Model =
    { content : String }

type Msg
    = GotText (Result Http.Error String)

init : ( Model, Cmd Msg )
init =
    ( Model ""
    , fetchPage "https://api.example.com/data"
    )

fetchPage : String -> Cmd Msg
fetchPage url =
    Http.get { url = url, expect = Http.expectString GotText }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText (Ok data) ->
            ( { model | content = data }, Cmd.none )

        GotText (Err _) ->
            ( { model | content = "ข้อผิดพลาด: ไม่สามารถดึงเพจได้." }, Cmd.none )

view : Model -> Html Msg
view model =
    text model.content

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }

```

หากการดึงข้อมูลสำเร็จ, `content` ในโมเดลของคุณจะมีเนื้อหาของเพจ หากเกิดข้อผิดพลาด, จะมีข้อความแสดงข้อผิดพลาดง่าย ๆ

## ศึกษาเพิ่มเติม

Elm มองผลกระทบด้านข้างเป็นข้อมูล, หมายความว่า คำขอ HTTP จะถูกจัดการโดยรันไทม์ของ Elm, ไม่ใช่ในโค้ดของคุณโดยตรง ในอดีต, นี่เป็นการเปลี่ยนแปลงจากภาษาอื่น ๆ เช่น JavaScript ที่ผลกระทบด้านข้างสามารถทำได้อย่างอิสระมากกว่า ทางเลือกในภาษาอื่นอาจเป็น `fetch` ใน JavaScript หรือ `requests` ของ Python Elm ใช้วิธีนี้เพื่อให้แอปของคุณอยู่ในสภาพที่คาดเดาได้และง่ายต่อการบำรุงรักษา โดยการเข้ารหัสผลกระทบด้านข้างเข้าไปในประเภทและใช้ฟังก์ชัน `update` กลางเพื่อจัดการการเปลี่ยนแปลง

โมดูล `Http` ไม่ได้มีอยู่เสมอไปใน Elm ในเวอร์ชั่นแรก ๆ ต้องใช้ AJAX ที่สร้างขึ้นเอง, ซึ่งเป็นเรื่องยุ่งยาก ตอนนี้, `Http` มีฟังก์ชันหลากหลายเพื่อจัดการกับกรณีต่าง ๆ, เช่น คาดหวัง JSON หรือสตริง, ทำให้ใช้งานง่ายขึ้น

ในการเขียนโค้ด, เมื่อคุณเรียก `fetchPage`, Elm จะส่งข้อความไปยังฟังก์ชัน `update` ของคุณพร้อมกับผลลัพธ์ จะเป็น `Ok data` หากสำเร็จ หรือ `Err error` หากล้มเหลว คุณจะใช้ pattern matching เพื่อจับผลลัพธ์เหล่านี้และอัพเดทโมเดลและมุมมองของคุณตามนั้น

## ดูเพิ่มเติม

- เอกสารของแพ็กเกจ HTTP ของ Elm: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- คู่มือ Elm สำหรับ Effects: [https://guide.elm-lang.org/effects/](https://guide.elm-lang.org/effects/)
- Decoding JSON ใน Elm (สำหรับเมื่อข้อมูลที่คุณกำลังดึงไม่ใช่สตริงธรรมดา): [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
