---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:48.761895-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E43\u0E19\u0E2D\u0E14\u0E35\
  \u0E15, \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\
  \u0E20\u0E32\u0E29\u0E32 C \u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\
  \u0E31\u0E19 `strlen()` \u0E08\u0E32\u0E01 `<string.h>`, \u0E19\u0E31\u0E1A\u0E15\
  \u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E08\u0E19\u0E01\u0E23\u0E30\u0E17\u0E31\
  \u0E48\u0E07\u0E1E\u0E1A\u0E15\u0E31\u0E27 null-terminator \u0E43\u0E19\u0E42\u0E25\
  \u0E01\u0E02\u0E2D\u0E07 Arduino, \u0E04\u0E25\u0E32\u0E2A `String`\u2026"
lastmod: '2024-04-05T22:51:14.614123-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19\u0E2D\u0E14\u0E35\u0E15, \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E20\u0E32\u0E29\u0E32 C \u0E43\u0E0A\u0E49\
  \u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `strlen()` \u0E08\u0E32\u0E01 `<string.h>`,\
  \ \u0E19\u0E31\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\u0E08\u0E19\
  \u0E01\u0E23\u0E30\u0E17\u0E31\u0E48\u0E07\u0E1E\u0E1A\u0E15\u0E31\u0E27 null-terminator\
  \ \u0E43\u0E19\u0E42\u0E25\u0E01\u0E02\u0E2D\u0E07 Arduino, \u0E04\u0E25\u0E32\u0E2A\
  \ `String` \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E0A\u0E35\u0E27\u0E34\u0E15\u0E07\u0E48\
  \u0E32\u0E22\u0E02\u0E36\u0E49\u0E19\u0E14\u0E49\u0E27\u0E22\u0E27\u0E34\u0E18\u0E35\
  \u0E01\u0E32\u0E23 `length()` \u0E17\u0E35\u0E48\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\
  \u0E41\u0E25\u0E49\u0E27 \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E01\u0E47\u0E15\
  \u0E32\u0E21, \u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 `String` \u0E2D\
  \u0E32\u0E08\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2B\u0E19\u0E48\u0E27\u0E22\u0E04\u0E27\
  \u0E32\u0E21\u0E08\u0E33\u0E08\u0E33\u0E01\u0E31\u0E14\u0E02\u0E2D\u0E07\u0E2D\u0E38\
  \u0E1B\u0E01\u0E23\u0E13\u0E4C\u0E01\u0E23\u0E30\u0E08\u0E32\u0E22\u0E15\u0E31\u0E27\
  \u0E40\u0E1B\u0E47\u0E19\u0E40\u0E27\u0E25\u0E32\u0E19\u0E32\u0E19 \u0E17\u0E32\u0E07\
  \u0E40\u0E25\u0E37\u0E2D\u0E01\u0E04\u0E37\u0E2D\u0E2D\u0E30\u0E44\u0E23?"
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
