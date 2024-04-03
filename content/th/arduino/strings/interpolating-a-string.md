---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:50.661823-06:00
description: "\u0E01\u0E32\u0E23\u0E43\u0E2A\u0E48\u0E04\u0E48\u0E32\u0E15\u0E31\u0E27\
  \u0E41\u0E1B\u0E23\u0E25\u0E07\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E04\u0E37\
  \u0E2D\u0E01\u0E32\u0E23\u0E1C\u0E2A\u0E21\u0E1C\u0E2A\u0E32\u0E19\u0E15\u0E31\u0E27\
  \u0E41\u0E1B\u0E23\u0E01\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21 \u0E40\
  \u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\u0E48\u0E19\u0E31\u0E01\u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E21\u0E31\u0E01\u0E17\u0E33\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E41\u0E1A\
  \u0E1A\u0E44\u0E14\u0E19\u0E32\u0E21\u0E34\u0E01\u0E41\u0E25\u0E30\u0E2D\u0E48\u0E32\
  \u0E19\u0E07\u0E48\u0E32\u0E22\u0E44\u0E14\u0E49\u0E43\u0E19\u0E02\u0E13\u0E30\u0E17\
  \u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E01\u0E33\u0E25\u0E31\u0E07\
  \u0E17\u0E33\u0E07\u0E32\u0E19"
lastmod: '2024-03-17T21:57:56.468314-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E43\u0E2A\u0E48\u0E04\u0E48\u0E32\u0E15\u0E31\u0E27\
  \u0E41\u0E1B\u0E23\u0E25\u0E07\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E04\u0E37\
  \u0E2D\u0E01\u0E32\u0E23\u0E1C\u0E2A\u0E21\u0E1C\u0E2A\u0E32\u0E19\u0E15\u0E31\u0E27\
  \u0E41\u0E1B\u0E23\u0E01\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21 \u0E40\
  \u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E17\u0E35\u0E48\u0E19\u0E31\u0E01\u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E21\u0E31\u0E01\u0E17\u0E33\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E41\u0E1A\
  \u0E1A\u0E44\u0E14\u0E19\u0E32\u0E21\u0E34\u0E01\u0E41\u0E25\u0E30\u0E2D\u0E48\u0E32\
  \u0E19\u0E07\u0E48\u0E32\u0E22\u0E44\u0E14\u0E49\u0E43\u0E19\u0E02\u0E13\u0E30\u0E17\
  \u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E01\u0E33\u0E25\u0E31\u0E07\
  \u0E17\u0E33\u0E07\u0E32\u0E19."
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 8
---

## วิธีการ:
Arduino ไม่มีฟีเจอร์การใส่ค่าตัวแปรลงในสตริงเป็นค่าปริยาย แต่คุณสามารถได้ผลลัพธ์ที่คล้ายคลึงกันโดยใช้ `sprintf()` หรือโดยการผูกสตริงและตัวแปรเข้าด้วยกัน

```Arduino
char buffer[50]; // ตรวจสอบให้แน่ใจว่าขนาดนี้เพียงพอที่จะจุสตริงสุดท้าย
int sensorValue = analogRead(A0);
sprintf(buffer, "Sensor reading: %d", sensorValue);
Serial.println(buffer);
```

ผลลัพธ์:
```
Sensor reading: 402
```

หรือโดยใช้การผูกสตริง:

```Arduino
String message = "Sensor reading: " + String(sensorValue);
Serial.println(message);
```

## ลงลึก:
ภาษา C และ C++ (ภาษาหลักของสคริปต์ Arduino) โดยปกติไม่มีการใส่ค่าตัวแปรลงในสตริงเหมือนภาษาใหม่ๆ เช่น Python หรือ JavaScript แทนที่จะใช้ `sprintf()`, ซึ่งเป็นวิธีที่พบบ่อยในการจัดรูปแบบสตริงด้วยตัวแปร มันทำงานได้ แต่อาจดูเกะกะและมีความเสี่ยงต่อการล้นของบัฟเฟอร์หากไม่จัดการอย่างระมัดระวัง

การผสานด้วยคลาส `String` นั้นเข้าใจง่ายกว่าและปลอดภัยกว่าจากข้อผิดพลาดด้านหน่วยความจำ ข้อเสีย? มันอาจนำไปสู่การแตกตัวของหน่วยความจำ, เฉพาะอย่างยิ่งในโปรแกรมที่ทำงานยาวนานบนอุปกรณ์ที่มีหน่วยความจำจำกัดเช่น Arduinos

ตัวเลือกหนึ่งในบางไลบรารี C++ ใหม่หรือเฉพาะทาง(ไม่เป็นมาตรฐานใน Arduino)คือการใช้ไลบรารีจัดรูปแบบสตริงที่ให้ไวยากรณ์ที่ใกล้เคียงกับการใส่ค่าตัวแปร, เช่น `fmtlib`

สำหรับรายละเอียดการประมวลผล, เมื่อคุณทำการผสานด้วยคลาส `String`, อะร์ดูโนจะสร้างวัตถุสตริงใหม่และจัดการหน่วยความจำให้คุณอยู่เบื้องหลัง `sprintf()`, ในทางกลับกัน, จะเขียนข้อความที่จัดรูปแบบไว้ลงในบัฟเฟอร์ที่คุณจัดสรรไว้, ให้คุณมีการควบคุมมากขึ้นแต่มีค่าใช้จ่ายในการต้องจัดการหน่วยความจำด้วยตนเอง

## ดูเพิ่มเติม
- คำอ้างอิงคลาส `String` ของ Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- คำอ้างอิงฟังก์ชัน `sprintf()`: http://www.cplusplus.com/reference/cstdio/sprintf/
- การเพิ่มประสิทธิภาพหน่วยความจำของ Arduino: https://www.arduino.cc/en/Tutorial/Foundations/Memory
- fmtlib, ไลบรารีจัดรูปแบบสตริงสมัยใหม่: https://fmt.dev/latest/index.html
