---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:00.406679-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48 Arduino \u0E02\u0E2D\u0E07\
  \u0E04\u0E38\u0E13\u0E2A\u0E37\u0E48\u0E2D\u0E2A\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E40\
  \u0E27\u0E47\u0E1A \u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E02\u0E2D\u0E43\
  \u0E2B\u0E49\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C\u0E2A\
  \u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E01\u0E25\u0E31\u0E1A\u0E21\u0E32\
  \ \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E41\u0E1A\u0E1A\
  \u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49 Arduino \u0E02\
  \u0E2D\u0E07\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A\u2026"
lastmod: '2024-03-17T21:57:56.478925-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48 Arduino \u0E02\u0E2D\u0E07\
  \u0E04\u0E38\u0E13\u0E2A\u0E37\u0E48\u0E2D\u0E2A\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E40\
  \u0E27\u0E47\u0E1A \u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E02\u0E2D\u0E43\
  \u0E2B\u0E49\u0E40\u0E0B\u0E34\u0E23\u0E4C\u0E1F\u0E40\u0E27\u0E2D\u0E23\u0E4C\u0E2A\
  \u0E48\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E01\u0E25\u0E31\u0E1A\u0E21\u0E32\
  \ \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E41\u0E1A\u0E1A\
  \u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E2B\u0E49 Arduino \u0E02\
  \u0E2D\u0E07\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E01\u0E31\u0E1A API, \u0E14\u0E36\u0E07\u0E40\
  \u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E40\u0E27\u0E47\u0E1A, \u0E2B\u0E23\u0E37\
  \u0E2D\u0E2A\u0E37\u0E48\u0E2D\u0E2A\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E1A\u0E23\u0E34\
  \u0E01\u0E32\u0E23\u0E2D\u0E34\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E40\u0E19\u0E47\
  \u0E15\u0E2D\u0E37\u0E48\u0E19\u0E46."
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## อะไร & ทำไม?

การส่งคำขอ HTTP คือวิธีที่ Arduino ของคุณสื่อสารกับเว็บ เช่น การขอให้เซิร์ฟเวอร์ส่งข้อมูลกลับมา นักพัฒนาทำแบบนี้เพื่อให้ Arduino ของพวกเขาสามารถโต้ตอบกับ API, ดึงเนื้อหาเว็บ, หรือสื่อสารกับบริการอินเทอร์เน็ตอื่นๆ

## วิธีการ:

การทำงานกับ Arduino ต้องใช้ไลบรารี `WiFiNINA` สำหรับคุณสมบัติเครือข่าย นี่คือวิธีการส่งคำขอ GET อย่างง่าย:

```Arduino
#include <WiFiNINA.h>

char ssid[] = "yourNetworkName";       // ชื่อเครือข่ายของคุณ
char pass[] = "yourNetworkPass";       // รหัสผ่านเครือข่ายของคุณ
int status = WL_IDLE_STATUS;           // สถานะของ WiFi
char server[] = "example.com";         // เซิร์ฟเวอร์ที่คุณต้องการเชื่อมต่อ

WiFiClient client;

void setup() {
  Serial.begin(9600);                  // เริ่มการสื่อสารผ่าน serial สำหรับการดีบัก
  WiFi.begin(ssid, pass);              // เริ่มการเชื่อมต่อ WiFi
  while (status != WL_CONNECTED) {     // รอการเชื่อมต่อ:
    status = WiFi.status();
    delay(1000);
  }
  Serial.print("Connected to ");
  Serial.println(ssid);
}

void loop() {
  if (client.connect(server, 80)) {    // ถ้าเชื่อมต่อสำเร็จ ส่งคำขอ:
    client.println("GET / HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();                   // สิ้นสุดคำขอ
  } else {
    Serial.println("Connection failed"); // ถ้าไม่สามารถเชื่อมต่อกับเซิร์ฟเวอร์:
  }

  while (client.connected()) {         // ขณะที่เชื่อมต่ออยู่ อ่านข้อมูล:
    if (client.available()) {
      char c = client.read();
      Serial.print(c);
    }
  }

  if (!client.connected()) {           // ถ้าเซิร์ฟเวอร์ตัดการเชื่อมต่อ หยุดคลายเนินต์:
    client.stop();
  }

  delay(10000);                        // รอสิบวินาทีก่อนลองอีกครั้ง
}
```

ตัวอย่างผลลัพธ์:
```
HTTP/1.1 200 OK
Date: Mon, 23 Jan 2023 12:36:47 GMT
Server: Apache/2.4.1 (Unix)
...
```

## ศึกษาเพิ่มเติม

แนวคิดเกี่ยวกับการส่งคำขอ HTTP จากไมโครคอนโทรลเลอร์ไม่เคยเป็นเรื่องที่มีอยู่มาก่อนเสมอไป ในอดีต ไมโครคอนโทรลเลอร์มักใช้กับเซ็นเซอร์และการโต้ตอบกับโลกแห่งความจริง แต่ด้วยการเกิดขึ้นของ IoT (Internet of Things) เหล่าอุปกรณ์เหล่านี้เริ่มจำเป็นต้องมีความสามารถเชื่อมต่อเว็บ Arduino สามารถใช้ไลบรารีอย่าง `WiFiNINA` เพื่อจัดการการเชื่อมต่อเหล่านี้ได้อย่างมั่นคง

มีตัวเลือกอื่นๆ นอกจาก `WiFiNINA` ขึ้นอยู่กับฮาร์ดแวร์ของคุณ ตัวอย่างเช่น ไลบรารี `Ethernet` ใช้กับการเชื่อมต่อแบบมีสาย ในขณะที่ `WiFi101` ใช้กับ WiFi shields เก่า

ในส่วนของการดำเนินการ, การทำคำขอ HTTP อาจดูเรียบง่าย แต่การทำของมือ, หัวข้อ, และวิธี HTTP (GET, POST ฯลฯ) เป็นส่วนหนึ่งของโปรโตคอลที่เข้มงวด ซึ่งอนุญาตให้อุปกรณ์สื่อสารกันผ่านเว็บได้ Arduino ทำให้ความซับซ้อนส่วนใหญ่เรื่องนี้เรียบง่ายขึ้น แต่การเข้าใจพื้นฐานช่วยในการแก้ไขปัญหาเมื่อสิ่งต่างๆ ไม่เป็นไปอย่างราบรื่น

## ดูเพิ่มเติม

- เอกสารไลบรารี Arduino `WiFiNINA`: https://www.arduino.cc/en/Reference/WiFiNINA
- แนะนำโปรโตคอล HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP
- ฮับโปรเจค Arduino สำหรับโปรเจคที่เชื่อมต่อกับเว็บ: https://create.arduino.cc/projecthub
