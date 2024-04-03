---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:28.464425-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: ."
lastmod: '2024-03-17T21:57:56.498381-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 22
---

## วิธีการ:
```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // รอการเชื่อมต่อพอร์ตซีเรียล
  }

  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }

  myFile = SD.open("example.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Error opening example.txt");
  }
}

void loop() {
  // ไม่มีอะไรเกิดขึ้นหลังจากการตั้งค่า
}
```

ผลลัพธ์ที่คาดหวังบนจอภาพซีเรียลจะเป็นเนื้อหาของ `example.txt` ถ้าทุกอย่างเชื่อมต่อและเริ่มต้นใช้งานได้อย่างถูกต้อง

## ลงลึกยิ่งขึ้น
ในอดีต, ไมโครคอนโทรลเลอร์เช่น Arduino มีหน่วยความจำเล็กน้อยและไม่สามารถจัดการกับไฟล์ได้ แต่ด้วยโมดูลการ์ด SD และหน่วยความจำในตัวที่ใหญ่ขึ้น เราจึงมีการทำงานกับไฟล์ได้ มีหลายไลบรารีที่มีไว้สำหรับวัตถุประสงค์นี้ เช่น `<SD.h>` มันสร้างขึ้นบนพื้นฐานของ `<SPI.h>` เพื่อการสื่อสารกับการ์ด SD ผ่านบัส SPI

ในแง่ของทางเลือกอื่น ๆ คุณอาจใช้ EEPROM (หน่วยความจำที่ไม่หายไป) สำหรับข้อมูลขนาดเล็ก หรือแม้กระทั่งเชื่อมต่อ Arduino กับเครือข่ายและดึงไฟล์จากเซิร์ฟเวอร์ได้ ไลบรารี `<SD.h>` เป็น wrapper สำหรับฟังก์ชันระดับต่ำ จัดการการจัดการไฟล์, การอ่าน, และการเขียนในแบบที่คล้ายกับสตรีม C++ มาตรฐาน

การปรับใช้ใน Arduino รวมถึงการเริ่มต้นใช้งานโมดูลการ์ด SD, การเปิดไฟล์, การอ่านจนกระทั่งไม่มีอะไรเหลืออีกต่อไป, และปิดมันเพื่อปลดปล่อยทรัพยากร เป็นสิ่งสำคัญที่จะต้องจัดการกับข้อผิดพลาด เช่น การล้มเหลวในการเริ่มต้นหรือการเปิดไฟล์ เพราะเป็นสาเหตุหลักของปัญหาในการทำงานกับไฟล์

## ดูเพิ่มเติม
- อ้างอิงไลบรารี SD อย่างเป็นทางการ: https://www.arduino.cc/en/Reference/SD
- ไลบรารี SPI ของ Arduino สำหรับการสื่อสารซีเรียล: https://www.arduino.cc/en/reference/SPI
- คู่มือการใช้ EEPROM กับ Arduino สำหรับงานจัดเก็บข้อมูลขนาดเล็ก: https://www.arduino.cc/en/Tutorial/LibraryExamples/EEPROMReadWrite
