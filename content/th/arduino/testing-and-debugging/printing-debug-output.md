---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:17.070849-06:00
description: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E2B\
  \u0E19\u0E49\u0E32\u0E08\u0E2D Serial \u0E40\u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\
  \u0E35\u0E01\u0E32\u0E23\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E39\u0E20\u0E32\u0E22\
  \u0E43\u0E19\u0E04\u0E27\u0E32\u0E21\u0E04\u0E34\u0E14\u0E02\u0E2D\u0E07 Arduino\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E01\u0E32\u0E23\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E32\
  \u0E21\u0E2B\u0E32\u0E1B\u0E31\u0E0D\u0E2B\u0E32, \u0E17\u0E14\u0E2A\u0E2D\u0E1A\
  \u0E2A\u0E21\u0E21\u0E15\u0E34\u0E10\u0E32\u0E19,\u2026"
lastmod: '2024-03-17T21:57:56.483995-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E2B\
  \u0E19\u0E49\u0E32\u0E08\u0E2D Serial \u0E40\u0E1B\u0E47\u0E19\u0E27\u0E34\u0E18\
  \u0E35\u0E01\u0E32\u0E23\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E39\u0E20\u0E32\u0E22\
  \u0E43\u0E19\u0E04\u0E27\u0E32\u0E21\u0E04\u0E34\u0E14\u0E02\u0E2D\u0E07 Arduino\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E01\u0E32\u0E23\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E32\
  \u0E21\u0E2B\u0E32\u0E1B\u0E31\u0E0D\u0E2B\u0E32, \u0E17\u0E14\u0E2A\u0E2D\u0E1A\
  \u0E2A\u0E21\u0E21\u0E15\u0E34\u0E10\u0E32\u0E19, \u0E41\u0E25\u0E30\u0E15\u0E23\
  \u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E27\u0E25\
  \u0E32\u0E08\u0E23\u0E34\u0E07\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\
  \u0E07\u0E43\u0E0A\u0E49\u0E40\u0E17\u0E04\u0E19\u0E34\u0E04\u0E14\u0E35\u0E1A\u0E31\
  \u0E01\u0E17\u0E35\u0E48\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27\u0E40\u0E01\u0E34\
  \u0E19\u0E44\u0E1B."
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
weight: 33
---

## อะไร & ทำไม?

การพิมพ์ข้อความดีบักไปยังหน้าจอ Serial เป็นวิธีการเพื่อดูภายในความคิดของ Arduino โปรแกรมเมอร์ทำการนี้เพื่อตามหาปัญหา, ทดสอบสมมติฐาน, และตรวจสอบข้อมูลเวลาจริงโดยไม่ต้องใช้เทคนิคดีบักที่รวดเร็วเกินไป

## วิธีทำ:

ไปตรงเรื่องเลย สมมติว่าคุณต้องการพิมพ์ "Hello, world!" ทุกๆวินาที นี่คือตัวอย่างโค้ด:

```Arduino
void setup() {
  Serial.begin(9600);  // เริ่มการสื่อสารทาง Serial
}

void loop() {
  Serial.println("Hello, world!");  // พิมพ์ข้อความ
  delay(1000);  // รอซักหนึ่งวินาที
}
```

เปิดหน้าจอ Serial Monitor ใน Arduino IDE และดูข้อความที่ปรากฏออกมาเรื่อยๆ ตัวอย่างผลลัพธ์:

```
Hello, world!
Hello, world!
Hello, world!
...
```

## ศึกษาลึกลงไป

ก่อนที่ `Serial` จะกลายมาเป็นเพื่อนคู่ใจ, ผู้คนใช้ไฟ LED กระพริบเพื่อสื่อสาร - ยุคหินของการดีบัก จากนั้น, เครื่องมือดีบักขั้นสูงก็ได้ปรากฏขึ้น, แต่มีราคาแพง `Serial.print()` และครอบครัวของมันทำให้เราสามารถส่งข้อความไปยังหน้าจอได้รวดเร็วและถูกต้อง

แนวทางเลือก? คุณมี LCD, การบันทึกลง SD การ์ด, หรือแม้แต่ Bluetooth สำหรับผู้ที่ไม่ชอบสายไฟ แต่ละวิธีมีข้อแตกต่าง; `Serial` เป็นแค่ตัวเลือกที่ตรงไปตรงมา - ง่าย, ตรงไปตรงมา, เสมอมา

ภายใน, `Serial.print()` แปลงข้อมูลของคุณเป็นไบต์ที่เคลื่อนผ่าน USB ไปยังคอมพิวเตอร์ของคุณ สิ่งนี้เกิดขึ้นผ่านพอร์ตซีเรียลที่ใช้ฮาร์ดแวร์ (UART) หรือซอฟต์แวร์จำลอง (SoftSerial) เชื่อถือได้, แต่การใช้พอร์ตมากเกินไปด้วยข้อมูลจำนวนมากอาจทำให้โปรแกรมของคุณหยุดนิ่ง, ดังนั้นให้ใช้การพิมพ์ซีเรียลเหมือนคุณกำลังปรุงรสสเต็ก, ไม่ใช่ท่วมซุป

## ดูเพิ่มเติม

สำหรับผู้ที่อยากศึกษาเพิ่ม:

- คู่มือของ Arduino เกี่ยวกับ `Serial`: [Arduino Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- สำหรับวิทยาศาสตร์เบื้องหลังการสื่อสารแบบซีเรียล: [UART Communication](https://www.sparkfun.com/tutorials/215)
