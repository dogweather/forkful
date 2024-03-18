---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:14.072518-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\
  \u0E25\u0E02\u0E2A\u0E38\u0E48\u0E21\u0E43\u0E19 Elm \u0E19\u0E31\u0E49\u0E19\u0E15\
  \u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\u0E39\u0E25 `Random` \u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\u0E25\
  \u0E02\u0E1B\u0E25\u0E2D\u0E21\u0E2A\u0E38\u0E48\u0E21 \u0E0B\u0E36\u0E48\u0E07\u0E21\
  \u0E35\u0E1B\u0E23\u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C\u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E07\u0E32\u0E19\u0E2B\u0E25\u0E32\u0E22\u0E2D\u0E22\u0E48\u0E32\u0E07 \u0E40\
  \u0E0A\u0E48\u0E19 \u0E40\u0E01\u0E21, \u0E01\u0E32\u0E23\u0E08\u0E33\u0E25\u0E2D\
  \u0E07,\u2026"
lastmod: '2024-03-17T21:57:56.124313-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\
  \u0E25\u0E02\u0E2A\u0E38\u0E48\u0E21\u0E43\u0E19 Elm \u0E19\u0E31\u0E49\u0E19\u0E15\
  \u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\u0E39\u0E25 `Random` \u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\u0E25\
  \u0E02\u0E1B\u0E25\u0E2D\u0E21\u0E2A\u0E38\u0E48\u0E21 \u0E0B\u0E36\u0E48\u0E07\u0E21\
  \u0E35\u0E1B\u0E23\u0E30\u0E42\u0E22\u0E0A\u0E19\u0E4C\u0E2A\u0E33\u0E2B\u0E23\u0E31\
  \u0E1A\u0E07\u0E32\u0E19\u0E2B\u0E25\u0E32\u0E22\u0E2D\u0E22\u0E48\u0E32\u0E07 \u0E40\
  \u0E0A\u0E48\u0E19 \u0E40\u0E01\u0E21, \u0E01\u0E32\u0E23\u0E08\u0E33\u0E25\u0E2D\
  \u0E07,\u2026"
title: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\u0E25\
  \u0E02\u0E2A\u0E38\u0E48\u0E21"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การสร้างตัวเลขสุ่มใน Elm นั้นต้องใช้โมดูล `Random` เพื่อสร้างตัวเลขปลอมสุ่ม ซึ่งมีประโยชน์สำหรับงานหลายอย่าง เช่น เกม, การจำลอง, และแม้กระทั่งเป็นส่วนหนึ่งของอัลกอริธึมที่ต้องการกระบวนการสุ่ม ความสามารถนี้ช่วยให้ผู้พัฒนาสามารถเพิ่มความไม่คาดคิดและความหลากหลายในแอปพลิเคชันของตน ช่วยพัฒนาประสบการณ์ผู้ใช้และฟังก์ชันการทำงาน

## วิธีการ:
ลักษณะธรรมชาติของ Elm ที่เป็นฟังก์ชันบริสุทธิ์หมายความว่าคุณไม่สามารถสร้างตัวเลขสุ่มโดยตรงเช่นเดียวกับในภาษาภาคบังคับได้ แทนที่จะใช้โมดูล `Random` ร่วมกับคำสั่ง นี่คือตัวอย่างพื้นฐานที่สร้างตัวเลขเต็มสุ่มระหว่าง 1 ถึง 100

ขั้นแรก ติดตั้งโมดูล `Random` ด้วย `elm install elm/random` จากนั้นนำเข้ามาในไฟล์ Elm ของคุณ พร้อมกับโมดูล HTML และเหตุการณ์ที่จำเป็น เช่นนี้:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

เพื่อให้ตัวอย่างนี้เป็นรายการที่สมบูรณ์, คุณสามารถเพิ่มโค้ดพื้นฐานนี้:
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

ต่อไป, กำหนด **คำสั่ง** เพื่อสร้างตัวเลขสุ่ม ซึ่งรวมถึงการตั้งค่าประเภท `Msg` เพื่อจัดการกับตัวเลขสุ่มเมื่อมันถูกสร้าง, ประเภท `Model` เพื่อเก็บข้อมูล, และฟังก์ชันอัพเดทเพื่อเชื่อมัติดต่อกัน
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

เพื่อที่จะเรียกใช้การสร้างตัวเลข คุณอาจจะส่งข้อความ `Generate` ผ่านปุ่มในมุมมองของคุณ ดังนี้:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Random Number: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Generate" ]
        ]
```

เมื่อคุณคลิกปุ่ม "Generate" ตัวเลขสุ่มระหว่าง 1 ถึง 100 จะถูกแสดงขึ้น

วิธีการง่ายๆ นี้สามารถปรับแต่งและขยายได้ โดยใช้ฟังก์ชันอื่นๆ ในโมดูล `Random` เพื่อสร้างทศนิยมสุ่ม, รายการสุ่ม, หรือแม้กระทั่งโครงสร้างข้อมูลซับซ้อนจากชนิดข้อมูลที่กำหนดเอง ซึ่งมอบพื้นที่การเล่นที่กว้างใหญ่สำหรับการเพิ่มความไม่คาดคิดในแอปพลิเคชัน Elm ของคุณ

คู่มือ Elm มีรายละเอียดเพิ่มเติม มันยังมี [ตัวอย่างการทอยลูกเต๋าหกด้าน](https://guide.elm-lang.org/effects/random)
