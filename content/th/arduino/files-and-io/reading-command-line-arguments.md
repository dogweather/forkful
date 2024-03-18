---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:29.908474-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\
  \u0E33\u0E2A\u0E31\u0E48\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E08\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E16\
  \u0E39\u0E01\u0E2A\u0E48\u0E07\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E37\u0E48\u0E2D\u0E04\u0E38\u0E13\u0E40\u0E23\u0E34\
  \u0E48\u0E21\u0E15\u0E49\u0E19\u0E08\u0E32\u0E01\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E21\
  \u0E34\u0E19\u0E31\u0E25\u0E2B\u0E23\u0E37\u0E2D\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\
  \u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E15\u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.496536-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\
  \u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\
  \u0E33\u0E2A\u0E31\u0E48\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E08\u0E31\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E16\
  \u0E39\u0E01\u0E2A\u0E48\u0E07\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E37\u0E48\u0E2D\u0E04\u0E38\u0E13\u0E40\u0E23\u0E34\
  \u0E48\u0E21\u0E15\u0E49\u0E19\u0E08\u0E32\u0E01\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E21\
  \u0E34\u0E19\u0E31\u0E25\u0E2B\u0E23\u0E37\u0E2D\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\
  \u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E15\u0E4C\u2026"
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การอ่านอาร์กิวเมนต์บรรทัดคำสั่งหมายถึงการจับข้อมูลที่ถูกส่งไปยังโปรแกรมเมื่อคุณเริ่มต้นจากเทอร์มินัลหรือคำสั่งพร้อมต์ โปรแกรมเมอร์ใช้อาร์กิวเมนต์ในการปรับแต่งพฤติกรรมของโปรแกรมโดยไม่ต้องเปลี่ยนโค้ด

## วิธีการ:
อาร์ดูอิโนไม่ทำงานกับอาร์กิวเมนต์บรรทัดคำสั่งเหมือนสภาพแวดล้อมการโปรแกรมแบบดั้งเดิม เพราะ sketch ถูกอัปโหลดไปยังไมโครคอนโทรลเลอร์โดยไม่มีบรรทัดคำสั่ง OS ที่เข้าถึงได้ แต่คุณสามารถเลียนแบบคุณสมบัตินี้โดยใช้การสื่อสารแบบซีเรียล นี่คือวิธีการ:

```arduino
void setup() {
  // ตั้งค่าการสื่อสารแบบซีเรียลที่ 9600 บิตต่อวินาที:
  Serial.begin(9600);
}

void loop() {
  // ตรวจสอบว่ามีข้อมูลที่พร้อมอ่านหรือไม่
  if (Serial.available() > 0) {
    // อ่านข้อมูลที่เข้ามาจนกระทั่งได้รับข้อความประกาศสิ้นสุดหรือ ‘\n’
    String receivedData = Serial.readStringUntil('\n');
    // สะท้อนอาร์กิวเมนต์ที่ได้รับกลับไปยังมอนิเตอร์ซีเรียล
    Serial.print("Received: ");
    Serial.println(receivedData);
  }
}
```

ตัวอย่างผลลัพธ์บน Serial Monitor:
```
Received: argument1 argument2 argument3
```

## ลึกซึ้ง
อาร์กิวเมนต์บรรทัดคำสั่งแบบดั้งเดิมทำงานในที่ที่ระบบปฏิบัติการเต็มรูปแบบ (เช่น Windows, Linux, หรือ macOS) ทำการรันโปรแกรม ตัวประมวลผลคำสั่งของระบบปฏิบัติการจะส่งอาร์กิวเมนต์ไปยังโปรแกรม อาร์ดูอิโนไม่มีสิ่งนี้; มันเป็นไมโครคอนโทรลเลอร์ที่มีโปรแกรมเดียวที่ทำงานซ้ำๆ

การสื่อสารแบบซีเรียลคือวิธีการอ้อมอาจของคุณ เหมือนกับการมีการสนทนากับอาร์ดูอิโนของคุณผ่านสายเฉพาะ คุณส่งข้อมูลผ่านสายนี้ซึ่งโปรแกรมอาร์ดูอิโนจะอ่านเป็นข้อมูลนำเข้าเมื่อพร้อม

ก่อนมี Serial Monitor ใน Arduino IDE โปรแกรมเมอร์ใช้สวิตช์หรือจัมเปอร์บนฮาร์ดแวร์เพื่อเปลี่ยนพฤติกรรม การสื่อสารแบบซีเรียลเปลี่ยนแปลงกระบวนการนี้อย่างมาก โดยทำให้กระบวนการนี้ง่ายขึ้น

ที่จำไป, Arduino Uno และอีกหลายตัวมีพอร์ตซีเรียลเพียงตัวเดียวที่ใช้ร่วมกับการเชื่อมต่อ USB ซึ่งหมายความว่าคุณไม่สามารถรับข้อมูลซีเรียลและอัปโหลดสเก็ตใหม่ได้พร้อมกัน บอร์ดอาร์ดูอิโนขั้นสูงอาจมีหลายพอร์ตซีเรียล ซึ่งอนุญาตให้มีการสื่อสารและอัปโหลดสเก็ตพร้อมกันได้

ทางเลือกอื่นๆ สำหรับการเลียนแบบอาร์กิวเมนต์บรรทัดคำสั่ง ได้แก่:

- โมดูล Bluetooth (สำหรับการสื่อสารไร้สาย)
- แป้นพิมพ์หรือปุ่มสำหรับการป้อนข้อมูล
- การบันทึกอาร์กิวเมนต์เข้าใน EEPROM (หน่วยความจำที่ไม่ถูกลบ) และอ่านพวกมันเมื่อเริ่มต้น

แต่ละวิธีมีกรณีการใช้งานและระดับความซับซ้อนต่างกัน แต่การสื่อสารแบบซีเรียลเป็นวิธีที่ง่ายที่สุดสำหรับการทำต้นแบบเร็ว ๆ และการทดสอบ

## ดูเพิ่มเติม
- การสื่อสารแบบซีเรียลของอาร์ดูอิโน: [Arduino - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- การอ่านและเขียน EEPROM ของอาร์ดูอิโน: [Arduino - EEPROM](https://www.arduino.cc/en/Reference/EEPROM)