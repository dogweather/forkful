---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:48.761895-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: ."
lastmod: '2024-03-17T21:57:56.473027-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\u0E02\
  \u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 7
---

## วิธีทำ:
```Arduino
void setup() {
  Serial.begin(9600); // เริ่มการสื่อสารแบบซีเรียล
  String myString = "Hello, Arduino!"; // สตริงของคุณที่นี่
  int stringLength = myString.length(); // ค้นหาความยาวของสตริง
  Serial.print("ความยาวของสตริงคือ: ");
  Serial.println(stringLength); // แสดงผลความยาว
}

void loop() {
  // ไม่มีอะไรต้องทำที่นี่
}
```
ตัวอย่างผลลัพธ์:
```
ความยาวของสตริงคือ: 15
```

## การทลายปัญหา
ในอดีต, โปรแกรมเมอร์ภาษา C ใช้ฟังก์ชัน `strlen()` จาก `<string.h>`, นับตัวอักษรจนกระทั่งพบตัว null-terminator ในโลกของ Arduino, คลาส `String` ทำให้ชีวิตง่ายขึ้นด้วยวิธีการ `length()` ที่มีอยู่แล้ว อย่างไรก็ตาม, การใช้งาน `String` อาจทำให้หน่วยความจำจำกัดของอุปกรณ์กระจายตัวเป็นเวลานาน ทางเลือกคืออะไร? ใช้อาร์เรย์ของ char (สตริงแบบสไตล์ C), ซึ่งเป็นมิตรกับหน่วยความจำมากกว่าแต่ค่อนข้างยากต่อการจัดการ

สำหรับโปรเจ็กต์ขนาดใหญ่, ควรคำนึงถึงการจัดการหน่วยความจำเสมอ ด้วยวิธีการ `length()`, ไม่ต้องการการคำนวณพิเศษ - วัตถุ `String` จะติดตามขนาดของมันเอง จากภายใน, `length()` เป็นการค้นหาอย่างรวดเร็ว, ไม่ใช่การนับตัวอักษร นั่นคือความมีประสิทธิภาพ! แต่ถ้าคุณมีหน่วยความจำน้อย, กลับไปที่พื้นฐานด้วยอาร์เรย์ของ char และการคำนวณความยาวด้วยตัวเอง, เหมือนกับวันวานของ `strlen()` นั่นเอง

## ดูเพิ่มเติม
- อ้างอิง Arduino `String`: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- ฟังก์ชัน Arduino `strlen()` สำหรับสตริงแบบสไตล์ C: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/
- การอภิปรายเรื่อง `String` กับอาร์เรย์ของ char ใน Arduino: https://forum.arduino.cc/t/string-vs-char-array/678207
