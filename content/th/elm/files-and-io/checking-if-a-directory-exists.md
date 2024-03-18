---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:53.724130-06:00
description: "\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\
  \u0E32\u0E21\u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\
  \u0E39\u0E48\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48\u0E2B\u0E21\u0E32\u0E22\u0E16\
  \u0E36\u0E07\u0E01\u0E32\u0E23\u0E22\u0E37\u0E19\u0E22\u0E31\u0E19\u0E27\u0E48\u0E32\
  \u0E21\u0E35\u0E40\u0E2A\u0E49\u0E19\u0E17\u0E32\u0E07\u0E42\u0E1F\u0E25\u0E40\u0E14\
  \u0E2D\u0E23\u0E4C\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\
  \u0E23\u0E30\u0E1A\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\
  \u0E48\u2026"
lastmod: '2024-03-17T21:57:56.143807-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\
  \u0E32\u0E21\u0E35\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2D\u0E22\
  \u0E39\u0E48\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48\u0E2B\u0E21\u0E32\u0E22\u0E16\
  \u0E36\u0E07\u0E01\u0E32\u0E23\u0E22\u0E37\u0E19\u0E22\u0E31\u0E19\u0E27\u0E48\u0E32\
  \u0E21\u0E35\u0E40\u0E2A\u0E49\u0E19\u0E17\u0E32\u0E07\u0E42\u0E1F\u0E25\u0E40\u0E14\
  \u0E2D\u0E23\u0E4C\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\
  \u0E23\u0E30\u0E1A\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\
  \u0E48\u2026"
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่หมายถึงการยืนยันว่ามีเส้นทางโฟลเดอร์เฉพาะอยู่ในระบบไฟล์หรือไม่ โปรแกรมเมอร์ทำเช่นนี้เพื่อหลีกเลี่ยงข้อผิดพลาดเมื่อเข้าถึง อ่าน หรือเขียนไฟล์

## วิธีการ:
Elm คือภาษาโปรแกรมมิ่งสำหรับเว็บด้านหน้า ดังนั้นจึงไม่มีการเข้าถึงระบบไฟล์โดยตรง อย่างไรก็ตาม คุณจะส่งคำสั่งไปยังบริการแบ็กเอนด์ใน JavaScript นี่คือวิธีการโครงสร้างการโต้ตอบดังกล่าวด้วย Elm:

```elm
port module Main exposing (..)

-- กำหนด port เพื่อสื่อสารกับ JavaScript
port checkDir : String -> Cmd msg

-- ตัวอย่างการใช้งาน
checkDirectory : String -> Cmd Msg
checkDirectory dir =
    checkDir dir
```

จากนั้น ใน JavaScript ของคุณ:

```javascript
app.ports.checkDir.subscribe(function(dir) {
    var exists = fs.existsSync(dir); // การใช้โมดูล 'fs' ของ Node เพื่อตรวจสอบไดเรกทอรี
    app.ports.dirExists.send(exists);
});
```

กลับไปที่ Elm เพื่อจัดการคำตอบ:

```elm
port dirExists : (Bool -> msg) -> Sub msg

type Msg = DirExists Bool

subscriptions : Model -> Sub Msg
subscriptions model =
    dirExists DirExists
```

หมายเหตุ: สิ่งนี้ต้องการการตั้งค่าพอร์ตและการจัดการแบ็กเอนด์อย่างเหมาะสมใน JavaScript

## ลงลึก
สภาพแวดล้อมที่จำกัดแค่เบราว์เซอร์ของ Elm หมายความว่ามันไม่สามารถเข้าถึงระบบไฟล์โดยตรง ไม่เหมือนกับ Node.js ในอดีต ภาษาฝั่งเซิร์ฟเวอร์และ Node.js ได้มอบฟังก์ชันการเข้าถึงระบบไฟล์ โดยภาษาฝั่งเบราว์เซอร์พึ่งพา API ของเซิร์ฟเวอร์เพื่อจัดการไฟล์ Elm ที่มีระบบประเภทข้อมูลที่เข้มงวดไม่ได้จัดการกับผลข้างเคียงเช่นการดำเนินการ I/O โดยตรง แต่ใช้พอร์ตสำหรับการทำงานร่วมกับ JavaScript ให้ Elm เองไม่สามารถตรวจสอบว่ามีไดเรกทอรีอยู่หรือไม่ แต่การใช้ Elm กับบริการแบ็กเอนด์ผ่านพอร์ตช่วยให้สามารถทำงานนี้ได้ในเว็บแอปพลิเคชัน

ในสภาพแวดล้อม Node.js ทางเลือกที่มีรวมถึงเมท็อด `fs.existsSync` หรือ `fs.access` สำหรับ Elm ควรพิจารณา Elm ฝั่งเซิร์ฟเวอร์กับแบ็กเอนด์เช่น `elm-serverless` ซึ่งสามารถจัดการกับการดำเนินการไฟล์ได้โดยตรงมากกว่า Elm ฝั่งไคลเอนต์

ในแง่ของการทำงาน เมื่อคุณตั้งค่าพอร์ตของคุณแล้ว แอป Elm ของคุณส่งข้อความไปยัง JavaScript ซึ่งทำการตรวจสอบระบบไฟล์ JavaScript จากนั้นส่งผลลัพธ์กลับไปยัง Elm นี้ช่วยรักษาความบริสุทธิ์และความปราศจากผลข้างเคียงของโค้ดฝั่งหน้าของ Elm รักษาหลักการสถาปัตยกรรมของมัน

## ดูเพิ่มเติม
- Elm คู่มือทางการเกี่ยวกับพอร์ต: https://guide.elm-lang.org/interop/ports.html
- เอกสารการใช้งานโมดูล `fs` ของ Node.js: https://nodejs.org/api/fs.html
- elm-serverless สำหรับการโต้ตอบ Elm ฝั่งเซิร์ฟเวอร์: https://package.elm-lang.org/packages/ktonon/elm-serverless/latest/
