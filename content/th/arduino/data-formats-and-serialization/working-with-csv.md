---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:35.242299-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV (Comma-Separated Values) \u0E43\u0E19 Arduino \u0E2B\u0E21\
  \u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E08\u0E32\
  \u0E01\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E25\u0E07\u0E44\u0E1B\u0E22\
  \u0E31\u0E07\u0E44\u0E1F\u0E25\u0E4C CSV \u0E17\u0E35\u0E48\u0E21\u0E31\u0E01\u0E08\
  \u0E30\u0E40\u0E01\u0E47\u0E1A\u0E2D\u0E22\u0E39\u0E48\u0E1A\u0E19 SD card \u0E0A\
  \u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E1A\u0E31\
  \u0E19\u0E17\u0E36\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:56.503220-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV (Comma-Separated Values) \u0E43\u0E19 Arduino \u0E2B\u0E21\
  \u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E08\u0E32\
  \u0E01\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E25\u0E07\u0E44\u0E1B\u0E22\
  \u0E31\u0E07\u0E44\u0E1F\u0E25\u0E4C CSV \u0E17\u0E35\u0E48\u0E21\u0E31\u0E01\u0E08\
  \u0E30\u0E40\u0E01\u0E47\u0E1A\u0E2D\u0E22\u0E39\u0E48\u0E1A\u0E19 SD card \u0E0A\
  \u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E1A\u0E31\
  \u0E19\u0E17\u0E36\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## คืออะไร และทำไม?
การทำงานกับไฟล์ CSV (Comma-Separated Values) ใน Arduino หมายถึงการอ่านจากและเขียนลงไปยังไฟล์ CSV ที่มักจะเก็บอยู่บน SD card ช่วยให้สามารถบันทึกข้อมูล, ตั้งค่าการกำหนดค่า และอื่นๆ ได้ โปรแกรมเมอร์มักจะจัดการกับ CSVs สำหรับการเก็บข้อมูลจากเซนเซอร์, การเก็บตัวแปรการกำหนดค่า หรือการเชื่อมต่อกับระบบอื่นๆ ด้วยความเรียบง่ายและการใช้กันอย่างกว้างขวางในหลายแพลตฟอร์ม

## วิธีการ:
Arduino ไม่มีไลบรารีภายในที่เฉพาะเจาะจงสำหรับการจัดการไฟล์ CSV แต่คุณสามารถใช้ `SD` และ `SPI` libraries สำหรับการเข้าถึงไฟล์บน SD card และจากนั้นคุณสามารถแยกหรือสร้างข้อมูล CSV โดยใช้เทคนิคการจัดการสตริงพื้นฐาน เมื่อต้องจัดการ CSV ที่ซับซ้อนมากขึ้น คุณสามารถใช้ไลบรารีจากบุคคลที่สาม `ArduinoCSV` สำหรับการแยกและเขียนได้ง่ายขึ้น

**การอ่านข้อมูล CSV จาก SD Card:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // พิมพ์บรรทัด CSV
    }
    dataFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // ไม่ได้ใช้ในตัวอย่างนี้
}
```
*ตัวอย่างผลลัพธ์:*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**การเขียนข้อมูล CSV ลงบน SD Card:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Value"); // ส่วนหัว CSV
    dataFile.println("1, 1597840923, 23.5"); // ตัวอย่างแถวข้อมูล
    dataFile.close();
    Serial.println("Data written");
  } else {
    Serial.println("Error opening output.csv");
  }
}

void loop() {
  // ไม่ได้ใช้ในตัวอย่างนี้
}
```
*ตัวอย่างผลลัพธ์:*
```
Data written
```

**การใช้ ArduinoCSV สำหรับการแยก:**
หากคุณต้องจัดการกับไฟล์ CSV ที่ซับซ้อน ArduinoCSV สามารถลดความพยายามในการแยกได้อย่างมาก ตัวอย่างนี้สมมติว่าคุณได้ติดตั้ง `ArduinoCSV` ไลบรารีเรียบร้อยแล้ว

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // พิมพ์แต่ละฟิลด์
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // ไม่ได้ใช้ในตัวอย่างนี้
}
```
*ตัวอย่างผลลัพธ์:*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
ในตัวอย่างเหล่านี้ โดยการอ่านจากและเขียนลงไปยังไฟล์ CSV บน SD card โครงการ Arduino สามารถเก็บข้อมูลได้อย่างง่ายดาย, เก็บการตั้งค่าการกำหนดค่า หรือแลกเปลี่ยนข้อมูลกับแอปพลิเคชันอื่นในรูปแบบที่เข้าถึงได้ทั่วไป
