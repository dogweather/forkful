---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:25.783558-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\
  \u0E07 standard error (stderr) \u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21 Arduino \u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E41\u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\
  \u0E14\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E27\u0E34\u0E19\u0E34\u0E08\u0E09\u0E31\
  \u0E22\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E0A\u0E48\u0E2D\u0E07\u0E17\u0E32\u0E07\u0E41\
  \u0E22\u0E01\u2026"
lastmod: '2024-03-17T21:57:56.497440-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\
  \u0E07 standard error (stderr) \u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21 Arduino \u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E41\u0E2A\u0E14\u0E07\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\
  \u0E14\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E27\u0E34\u0E19\u0E34\u0E08\u0E09\u0E31\
  \u0E22\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E0A\u0E48\u0E2D\u0E07\u0E17\u0E32\u0E07\u0E41\
  \u0E22\u0E01\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
weight: 25
---

## อะไรและทำไม?

การเขียนไปยัง standard error (stderr) ในการเขียนโปรแกรม Arduino หมายถึงการนำข้อความแสดงข้อผิดพลาดและการวินิจฉัยไปยังช่องทางแยก ทำให้สามารถแน่ใจได้ว่าไม่ผสมกับ standard output (stdout) โปรแกรมเมอร์ทำเช่นนี้เพื่อแยกความแตกต่างระหว่างผลลัพธ์ปกติของโปรแกรมออกจากข้อความแสดงข้อผิดพลาด ทำให้การแก้ไขข้อผิดพลาดและการวิเคราะห์บันทึกง่ายขึ้น

## วิธีการ:

Arduino โดยพื้นฐานแล้วไม่มีการแยกแยะระหว่าง standard output และ standard error เหมือนกับระบบคอมพิวเตอร์แบบเดิม ทั้ง `Serial.print()` และ `Serial.println()` เขียนไปยัง serial output เดียวกัน ซึ่งโดยปกติจะดูได้ใน Arduino IDE Serial Monitor อย่างไรก็ตาม เราสามารถเลียนแบบการเขียนไปยัง stderr โดยการจัดรูปแบบข้อความแสดงข้อผิดพลาดอย่างเฉพาะเจาะจงหรือนำไปยังเอาท์พุตอื่น เช่น ไฟล์บน SD card หรือผ่านการเชื่อมต่อเครือข่าย

เพื่อเลียนแบบ stderr คุณสามารถเติมคำนำหน้าข้อความแสดงข้อผิดพลาดด้วยแท็กเช่น "ERROR:" เพื่อแยกความแตกต่างใน Serial Monitor:

```cpp
void setup() {
  Serial.begin(9600); // เริ่มการสื่อสารแบบ serial ที่อัตราบอด 9600
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // การเลียนแบบ stderr โดยการเติมคำนำหน้าข้อความแสดงข้อผิดพลาด
    Serial.println("ERROR: ฟังก์ชันไม่สามารถปฏิบัติการได้.");
  } else {
    Serial.println("ฟังก์ชันปฏิบัติการสำเร็จ.");
  }
  delay(1000); // รอหนึ่งวินาทีก่อนเริ่ม loop ใหม่
}

int someFunction() {
  // ฟังก์ชันปลอมที่ส่งกลับ -1 ในกรณีข้อผิดพลาด
  return -1;
}
```

ตัวอย่างผลลัพธ์ใน Arduino IDE Serial Monitor อาจดูเช่นนี้:

```
ERROR: ฟังก์ชันไม่สามารถปฏิบัติการได้.
```

สำหรับโปรเจกต์ที่ต้องการวิธีการที่ซับซ้อนกว่า รวมถึงการเขียนไปยังเอาท์พุตทางกายภาพที่แตกต่างกัน อาจจำเป็นต้องใช้ไลบรารีของบุคคลที่สามหรือฮาร์ดแวร์เพิ่มเติม เช่น การบันทึกข้อความแสดงข้อผิดพลาดไปยัง SD card ต้องใช้ไลบรารี `SD`:

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("ERROR: เริ่มต้น SD card ล้มเหลว!");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("ERROR: ฟังก์ชันไม่สามารถปฏิบัติการได้.");
    myFile.close(); // ตรวจสอบให้แน่ใจว่าปิดไฟล์เพื่อบันทึกเนื้อหา
  } else {
    Serial.println("ERROR: เปิดไฟล์ error.log ล้มเหลว!");
  }
}

void loop() {
  // โค้ดหลักของคุณจะอยู่ที่นี่
}
```

ด้วยวิธีนี้ คุณแยกผลลัพธ์ปกติของโปรแกรมและข้อความแสดงข้อผิดพลาดโดยสิ้นเชิงโดยการนำหลังคาไปยังไฟล์ `error.log` บน SD card ทำให้สามารถวิเคราะห์หลังเกิดเหตุได้โดยไม่ทำให้ช่องทางเอาท์พุตหลักเกะกะ
