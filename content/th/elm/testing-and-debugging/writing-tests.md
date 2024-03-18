---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:14.689301-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E43\u0E19 Elm \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\
  \u0E07\u0E01\u0E23\u0E13\u0E35\u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E16\u0E39\
  \u0E01\u0E15\u0E49\u0E2D\u0E07\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14 Elm \u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E42\u0E14\u0E22\u0E43\u0E2B\u0E49\u0E21\u0E31\u0E48\
  \u0E19\u0E43\u0E08\u0E27\u0E48\u0E32\u0E21\u0E31\u0E19\u0E17\u0E33\u0E07\u0E32\u0E19\
  \u0E15\u0E32\u0E21\u0E17\u0E35\u0E48\u0E04\u0E32\u0E14\u0E2B\u0E27\u0E31\u0E07\u2026"
lastmod: '2024-03-17T21:57:56.132672-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E43\u0E19 Elm \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\
  \u0E07\u0E01\u0E23\u0E13\u0E35\u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E16\u0E39\
  \u0E01\u0E15\u0E49\u0E2D\u0E07\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14 Elm \u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E42\u0E14\u0E22\u0E43\u0E2B\u0E49\u0E21\u0E31\u0E48\
  \u0E19\u0E43\u0E08\u0E27\u0E48\u0E32\u0E21\u0E31\u0E19\u0E17\u0E33\u0E07\u0E32\u0E19\
  \u0E15\u0E32\u0E21\u0E17\u0E35\u0E48\u0E04\u0E32\u0E14\u0E2B\u0E27\u0E31\u0E07\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E01\u0E32\u0E23\u0E17\u0E14\
  \u0E2A\u0E2D\u0E1A"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การเขียนทดสอบใน Elm คือการสร้างกรณีทดสอบเพื่อตรวจสอบความถูกต้องของโค้ด Elm ของคุณ โดยให้มั่นใจว่ามันทำงานตามที่คาดหวัง โปรแกรมเมอร์ทำสิ่งนี้เพื่อจับบักได้ตั้งแต่เนิ่นๆ ทำให้การบำรุงรักษาง่ายขึ้น และเพิ่มคุณภาพและความน่าเชื่อถือของแอปพลิเคชันของพวกเขา

## วิธีการ:

Elm ใช้แพ็กเกจ `elm-explorations/test` สำหรับเขียนทดสอบหน่วย (unit tests) และทดสอบแบบฟัซซิ่ง (fuzz tests) เริ่มต้นด้วยการเพิ่มแพ็กเกจนี้เข้าไปในโปรเจคของคุณ:

```elm
elm install elm-explorations/test
```

สร้างไฟล์ทดสอบ ขอบอกว่า `tests/ExampleTest.elm` และนำเข้าโมดูลที่ใช้ในการทดสอบ นี่คือทดสอบง่าย ๆ ที่ตรวจสอบฟังก์ชัน `add : Int -> Int -> Int`:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "A simple addition function"
        [ test "Adding 2 and 3 yields 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

เพื่อรันทดสอบของคุณ คุณจะต้องมี `elm-test`:

```shell
npm ติดตั้ง -g elm-test
elm-test
```

สิ่งนี้จะคอมไพล์ทดสอบของคุณและพิมพ์ผลลัพธ์ในเทอร์มินัลของคุณ สำหรับตัวอย่างข้างต้น ผลลัพธ์ควรจะเป็นประมาณนี้:

```
การรันทดสอบ ผ่าน

ระยะเวลา: 42 มิลลิวินาที
ผ่าน:   1
ล้มเหลว:   0
```

สำหรับตัวอย่างที่ซับซ้อนขึ้น เราจะบอกว่าคุณต้องการทดสอบฟัซซิ่งฟังก์ชัน `add` เพื่อให้แน่ใจว่ามันจัดการกับช่วงของข้อมูลป้อนเข้าที่หลากหลายได้อย่างถูกต้อง คุณจะปรับเปลี่ยน `ExampleTest.elm` ของคุณดังนี้:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Testing add with fuzzing"
        [ fuzz int "Fuzz testing add with random ints" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

รัน `elm-test` อีกครั้งเพื่อดูการทดสอบฟัซซิ่งในการทำงาน ผลลัพธ์จะแตกต่างกันไปตามข้อมูลป้อนเข้าแบบสุ่ม แต่การทดสอบที่สำเร็จจะแสดงว่าไม่มีการล้มเหลว:

```
การรันทดสอบ ผ่าน

ระยะเวลา: 183 มิลลิวินาที
ผ่าน:   100
ล้มเหลว:   0
``` 

ตัวอย่างเหล่านี้แสดงวิธีการเขียนและรันทดสอบหน่วยและทดสอบฟัซซิ่งใน Elm โดยใช้แพ็กเกจ `elm-explorations/test` การทดสอบเป็นส่วนสำคัญของกระบวนการพัฒนา ช่วยให้มั่นใจว่าแอปพลิเคชัน Elm ของคุณเชื่อถือได้และรักษาคุณภาพสูง
