---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:51.588531-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E14\u0E49\u0E27\u0E22\
  \ Arduino IDE, \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\
  \u0E49 Serial prints \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E14\u0E1A\u0E31\u0E01\
  \u0E44\u0E14\u0E49 \u0E41\u0E15\u0E48\u0E01\u0E47\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\
  \u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E44\u0E1F\u0E09\u0E32\u0E22\
  \u0E2A\u0E33\u0E23\u0E27\u0E08\u0E16\u0E49\u0E33 \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E40\u0E14\u0E1A\u0E31\u0E01\u0E08\u0E23\u0E34\u0E07,\u2026"
lastmod: '2024-03-17T21:57:56.485872-06:00'
model: gpt-4-0125-preview
summary: "\u0E14\u0E49\u0E27\u0E22 Arduino IDE, \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49 Serial prints \u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E40\u0E14\u0E1A\u0E31\u0E01\u0E44\u0E14\u0E49 \u0E41\u0E15\u0E48\u0E01\u0E47\u0E40\
  \u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\
  \u0E44\u0E1F\u0E09\u0E32\u0E22\u0E2A\u0E33\u0E23\u0E27\u0E08\u0E16\u0E49\u0E33 \u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E14\u0E1A\u0E31\u0E01\u0E08\
  \u0E23\u0E34\u0E07, \u0E04\u0E38\u0E13\u0E2D\u0E32\u0E08\u0E2D\u0E22\u0E32\u0E01\
  \u0E08\u0E30\u0E22\u0E01\u0E23\u0E30\u0E14\u0E31\u0E1A\u0E40\u0E01\u0E21\u0E02\u0E2D\
  \u0E07\u0E04\u0E38\u0E13\u0E14\u0E49\u0E27\u0E22\u0E2D\u0E30\u0E44\u0E23\u0E2A\u0E31\
  \u0E01\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E0A\u0E48\u0E19 Atmel-ICE debugger\
  \ \u0E0B\u0E36\u0E48\u0E07\u0E23\u0E27\u0E21\u0E40\u0E02\u0E49\u0E32\u0E01\u0E31\
  \u0E1A\u0E2A\u0E20\u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21 Arduino\
  \ \u0E15\u0E48\u0E2D\u0E44\u0E1B\u0E19\u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E23\
  \u0E2A\u0E0A\u0E32\u0E15\u0E34\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E40\u0E14\u0E1A\
  \u0E31\u0E01\u0E41\u0E1A\u0E1A\u0E1B\u0E25\u0E2D\u0E21\u0E46 \u0E42\u0E14\u0E22\u0E43\
  \u0E0A\u0E49 Serial."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
weight: 35
---

## วิธีการ:
ด้วย Arduino IDE, คุณสามารถใช้ Serial prints เพื่อเดบักได้ แต่ก็เหมือนกับการใช้ไฟฉายสำรวจถ้ำ สำหรับการเดบักจริง, คุณอาจอยากจะยกระดับเกมของคุณด้วยอะไรสักอย่างเช่น Atmel-ICE debugger ซึ่งรวมเข้ากับสภาพแวดล้อม Arduino ต่อไปนี้เป็นรสชาติของการเดบักแบบปลอมๆ โดยใช้ Serial:

```Arduino
void setup() {
  Serial.begin(9600);
}
void loop() {
  int sensorValue = analogRead(A0);
  Serial.print("ค่าเซนเซอร์: ");
  Serial.println(sensorValue);
  // จินตนาการว่าคุณกำลังคาดหวัง 512 ที่นี่ แต่ได้ 0
  // เวลาในการตรวจสอบการเชื่อมต่อเซนเซอร์
  delay(1000); // รอเป็นเวลาหนึ่งวินาทีก่อนที่จะอ่านอีกครั้ง
}
```
เรียกใช้นี้ด้วย Serial Monitor เปิดอยู่ และคุณจะเห็นสิ่งที่เซ็นเซอร์ของคุณผลิตออกมาในเวลาจริง

## ดำดิ่งลึก
ก่อนที่จะมีเดบักเกอร์ เป็นโลกของการใช้คำสั่งพิมพ์ - คุณสามารถเดาได้เพียงว่าอะไรกำลังเกิดขึ้นโดยการพิมพ์ทุกอย่างออกมา เดบักด้วยคำสั่งพิมพ์ยังคงเป็นที่นิยม โดยเฉพาะในสภาพแวดล้อมที่ง่ายกว่าหรือในฮาร์ดแวร์ที่มีข้อจำกัดเช่น Arduino

ทางเลือกเพื่อการใช้เอมิเลเตอร์ในวงจรเช่น Atmel-ICE รวมถึงเครื่องมือเดบักซอฟต์แวร์เช่น `avr-gdb` คุณสามารถจับคู่มันกับ `avarice` เพื่อสร้างสะพานระหว่าง GDB กับฮาร์ดแวร์ของคุณซึ่งเป็นสิ่งที่สำหรับการเดบักขั้นสูงโดยตรงบนชิป

โดยใช้เดบักเกอร์, คุณสามารถตั้งจุดหยุด (breakpoints) เพื่อหยุดการดำเนินการที่จุดหลักๆ คุณสามารถเดินรหัสของคุณทีละบรรทัด, ตรวจสอบหน่วยความจำ, รีจีสเตอร์, และตัวแปร สิ่งนี้ช่วยให้คุณชี้แจงปัญหาแทนที่จะกระทำโดยคาดการณ์ เมื่อใช้เดบักเกอร์ ตรวจสอบให้แน่ใจว่าสภาพแวดล้อมของคุณตั้งค่าอย่างถูกต้อง - รุ่นที่ไม่ตรงกันหรือเครื่องมือที่ตั้งค่าไม่ถูกต้องอาจทำให้เกิดความผิดหวัง

## ดูเพิ่มเติม
พร้อมที่จะดำดิ่งลึกขึ้น? สำรวจสิ่งเหล่านี้:
- คู่มือการเดบัก Arduino ที่ [Arduino Debugging](https://www.arduino.cc/en/Guide/Environment#toc7)
- คู่มืออ้างอิง AVR Libc สำหรับการตั้งค่า avr-gdb: [หน้าหลัก AVR Libc](http://www.nongnu.org/avr-libc/)
