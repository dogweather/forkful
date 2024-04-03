---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:55.060392-06:00
description: null
lastmod: '2024-03-17T21:57:56.149943-06:00'
model: gpt-4-0125-preview
summary: null
title: "\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E0A\u0E31\u0E48\u0E27\
  \u0E04\u0E23\u0E32\u0E27"
weight: 21
---

# วิธีการ:
Elm ทำงานในเบราว์เซอร์ ดังนั้นจึงไม่มีสิทธิ์เข้าถึงระบบไฟล์โดยตรง ดังนั้นคุณไม่สามารถสร้างไฟล์ชั่วคราวแบบดั้งเดิมได้ แต่ถ้าคุณต้องการฟีเจอร์ที่คล้ายกัน เราใช้พอร์ตของ Elm เพื่อโต้ตอบกับ JavaScript ซึ่งสามารถจัดการการสร้างไฟล์ชั่วคราวได้

```elm
port module Main exposing (..)

-- กำหนดพอร์ตสำหรับการสร้างไฟล์ชั่วคราวใน JavaScript
port createTempFile : String -> Cmd msg

-- ส่งข้อมูลไปยัง JavaScript เพื่อสร้างไฟล์ชั่วคราว
saveDataTemporarily : String -> Cmd msg
saveDataTemporarily data =
    createTempFile data
```

สำหรับส่วนของ JavaScript โดยใช้ File API:

```javascript
app.ports.createTempFile.subscribe(function(data) {
    var blob = new Blob([data], {type: 'text/plain'});
    var url = URL.createObjectURL(blob);

    // ที่นี่คุณสามารถใช้ URL เพื่อดาวน์โหลด blob หรือส่งต่อไปยังส่วนอื่น ๆ ของแอปของคุณ
    console.log(url);  // มันจะบันทึก URL ของไฟล์ชั่วคราว
});
```

ตัวอย่างผลลัพธ์ในคอนโซล JavaScript:

```plaintext
blob:null/2135a9b7-1aad-4e7a-8bce-19c4f3f6d7ff
```

# ลงลึก
Elm ถูกออกแบบมาเพื่อความปลอดภัยและน่าเชื่อถือ ดังนั้นการเข้าถึงไฟล์ระบบโดยตรงไม่ได้อยู่ในแผน แทนที่จะใช้พอร์ตของ Elm เพื่อติดต่อกับ JavaScript ทำให้สามารถดำเนินการเช่นการสร้างไฟล์ชั่วคราวได้ ในประวัติศาสตร์ เราจัดการกับงานที่ขึ้นอยู่กับไฟล์ในเบราว์เซอร์ผ่าน JavaScript APIs โดยใช้ Elm สำหรับตรรกะระดับสูงที่ปลอดภัยตามประเภท

ในอนาคต WebAssembly อาจอนุญาตให้มีการโต้ตอบกับระบบไฟล์ได้โดยตรงมากขึ้น แต่ในขณะนี้ การใช้งานร่วมกับ JavaScript เป็นแนวปฏิบัติมาตรฐาน

ในแง่ของการดำเนินการ การสร้างไฟล์ชั่วคราวในบริบทของเบราว์เซอร์ไม่ได้หมายความถึงไฟล์จริงบนระบบไฟล์ แต่เป็นการเป็นตัวแทนในหน่วยความจำ (blob) ที่คุณสามารถทำงานได้และบันทึกตามความจำเป็น

# ดูเพิ่มเติม
- [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
- [MDN - Web APIs - File](https://developer.mozilla.org/en-US/docs/Web/API/File)
- [MDN - Web APIs - Blob](https://developer.mozilla.org/en-US/docs/Web/API/Blob)
