---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:41.573643-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E19 Arduino \u0E40\u0E01\u0E35\
  \u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E1A\
  \u0E31\u0E19\u0E17\u0E36\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E25\u0E07\u0E44\
  \u0E1F\u0E25\u0E4C\u0E1A\u0E19\u0E01\u0E32\u0E23\u0E4C\u0E14 SD \u0E2B\u0E23\u0E37\
  \u0E2D\u0E42\u0E21\u0E14\u0E39\u0E25\u0E08\u0E31\u0E14\u0E40\u0E01\u0E47\u0E1A\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E37\u0E48\u0E19\u0E46\u2026"
lastmod: '2024-03-17T21:57:56.499322-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E43\u0E19 Arduino \u0E40\u0E01\u0E35\
  \u0E48\u0E22\u0E27\u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E1A\
  \u0E31\u0E19\u0E17\u0E36\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E25\u0E07\u0E44\
  \u0E1F\u0E25\u0E4C\u0E1A\u0E19\u0E01\u0E32\u0E23\u0E4C\u0E14 SD \u0E2B\u0E23\u0E37\
  \u0E2D\u0E42\u0E21\u0E14\u0E39\u0E25\u0E08\u0E31\u0E14\u0E40\u0E01\u0E47\u0E1A\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2D\u0E37\u0E48\u0E19\u0E46\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 24
---

## อะไรและทำไม?
การเขียนไฟล์ข้อความใน Arduino เกี่ยวข้องกับการบันทึกข้อมูลลงไฟล์บนการ์ด SD หรือโมดูลจัดเก็บข้อมูลอื่นๆ เป็นส่วนใหญ่มักใช้สำหรับวัตถุประสงค์ในการบันทึกข้อมูล นักโปรแกรมทำสิ่งนี้เพื่อบันทึกการอ่านค่าจากเซ็นเซอร์ บันทึกการตั้งค่า หรือบันทึกเหตุการณ์ของแอปพลิเคชันตลอดเวลา ทำให้มันสำคัญสำหรับโครงการที่ต้องการการวิเคราะห์ข้อมูลหรือการติดตาม

## วิธีการ:
เพื่อเขียนไฟล์ข้อความลงบนการ์ด SD โดยใช้ Arduino คุณต้องรวมไลบรารี `SD.h` ซึ่งให้ฟังก์ชันที่จำเป็นในการโต้ตอบกับการ์ด SD ให้แน่ใจว่าบอร์ด Arduino ของคุณเชื่อมต่อกับโมดูลการ์ด SD

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // Initialize serial communication at 9600 bits per second:
  Serial.begin(9600);
  
  // Check for SD card initialization
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  Serial.println("Initialization done.");
  
  // Open the file. Note that only one file can be open at a time,
  // so you have to close this one before opening another.
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // If the file opened okay, write to it:
  if (myFile) {
    Serial.print("Writing to test.txt...");
    myFile.println("Testing text file write.");
    // Close the file:
    myFile.close();
    Serial.println("done.");
  } else {
    // If the file didn't open, print an error:
    Serial.println("Error opening test.txt");
  }
}

void loop() {
  // Nothing happens after setup
}
```

### ตัวอย่างผลลัพธ์:
เมื่อคุณรันโค้ดนี้ IDE ของ Arduino จะแสดง:
```
Initialization done.
Writing to test.txt...done.
```
เพื่อตรวจสอบว่าข้อมูลถูกเขียนอย่างถูกต้อง คุณสามารถถอดการ์ด SD จาก Arduino ใส่เข้าไปในคอมพิวเตอร์และเปิดไฟล์ `test.txt` เพื่อดูข้อความ "Testing text file write."

สำหรับโครงการที่ต้องการการดำเนินการกับไฟล์หรือการประมวลผลขั้นสูง พิจารณาสำรวจไลบรารีเพิ่มเติมหรือเขียนฟังก์ชันที่กำหนดเองเพื่อตอบโจทย์ความต้องการของคุณเอง
