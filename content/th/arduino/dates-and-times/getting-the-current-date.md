---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:07.145952-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Arduino \u0E40\u0E2D\u0E07\
  \u0E44\u0E21\u0E48\u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E43\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E36\u0E07\u0E27\u0E31\u0E19\
  \u0E17\u0E35\u0E48\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\u0E42\u0E14\u0E22\
  \u0E15\u0E23\u0E07 \u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01\u0E44\u0E21\
  \u0E48\u0E21\u0E35\u0E19\u0E32\u0E2C\u0E34\u0E01\u0E32\u0E08\u0E23\u0E34\u0E07 (RTC)\
  \ \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E01\u0E47\u0E15\u0E32\u0E21 \u0E2A\
  \u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E43\
  \u0E0A\u0E49\u0E42\u0E21\u0E14\u0E39\u0E25 RTC \u0E20\u0E32\u0E22\u0E19\u0E2D\u0E01\
  \ \u0E40\u0E0A\u0E48\u0E19\u2026"
lastmod: '2024-04-05T21:54:02.337004-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u0E40\u0E2D\u0E07\u0E44\u0E21\u0E48\u0E21\u0E35\u0E27\u0E34\u0E18\
  \u0E35\u0E01\u0E32\u0E23\u0E43\u0E19\u0E15\u0E31\u0E27\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E14\u0E36\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\u0E31\u0E08\u0E08\u0E38\
  \u0E1A\u0E31\u0E19\u0E42\u0E14\u0E22\u0E15\u0E23\u0E07 \u0E40\u0E19\u0E37\u0E48\u0E2D\
  \u0E07\u0E08\u0E32\u0E01\u0E44\u0E21\u0E48\u0E21\u0E35\u0E19\u0E32\u0E2C\u0E34\u0E01\
  \u0E32\u0E08\u0E23\u0E34\u0E07 (RTC) \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E01\
  \u0E47\u0E15\u0E32\u0E21 \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\u0E14\
  \u0E49\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E42\u0E21\u0E14\u0E39\u0E25 RTC \u0E20\
  \u0E32\u0E22\u0E19\u0E2D\u0E01 \u0E40\u0E0A\u0E48\u0E19 DS3231 \u0E41\u0E25\u0E30\
  \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E48\u0E40\u0E0A\u0E48\u0E19 `RTClib`\
  \ \u0E17\u0E35\u0E48\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E42\u0E14\u0E22 Adafruit \u0E0B\
  \u0E36\u0E48\u0E07\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E40\u0E0A\u0E37\
  \u0E48\u0E2D\u0E21\u0E15\u0E48\u0E2D\u0E01\u0E31\u0E1A\u0E42\u0E21\u0E14\u0E39\u0E25\
  \u0E40\u0E2B\u0E25\u0E48\u0E32\u0E19\u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\
  \u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22 \u0E01\u0E48\u0E2D\u0E19\u0E2D\u0E37\
  \u0E48\u0E19 \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E44\u0E14\
  \u0E49\u0E15\u0E34\u0E14\u0E15\u0E31\u0E49\u0E07\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\
  \u0E35 `RTClib` \u0E43\u0E19 Arduino IDE \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E41\
  \u0E25\u0E49\u0E27 \u0E08\u0E32\u0E01\u0E19\u0E31\u0E49\u0E19 \u0E40\u0E0A\u0E37\
  \u0E48\u0E2D\u0E21\u0E15\u0E48\u0E2D\u0E42\u0E21\u0E14\u0E39\u0E25 RTC \u0E02\u0E2D\
  \u0E07\u0E04\u0E38\u0E13\u0E01\u0E31\u0E1A Arduino \u0E15\u0E32\u0E21\u0E04\u0E39\
  \u0E48\u0E21\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 \u0E19\
  \u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\
  \u0E48\u0E32\u0E22\u0E46\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E23\u0E34\u0E48\u0E21\
  \u0E15\u0E49\u0E19."
title: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19"
weight: 29
---

## วิธีการ:
Arduino เองไม่มีวิธีการในตัวเพื่อดึงวันที่ปัจจุบันโดยตรง เนื่องจากไม่มีนาฬิกาจริง (RTC) อย่างไรก็ตาม สามารถทำได้โดยใช้โมดูล RTC ภายนอก เช่น DS3231 และไลบรารี่เช่น `RTClib` ที่พัฒนาโดย Adafruit ซึ่งทำให้การเชื่อมต่อกับโมดูลเหล่านี้เป็นเรื่องง่าย

ก่อนอื่น ตรวจสอบว่าได้ติดตั้งไลบรารี `RTClib` ใน Arduino IDE ของคุณแล้ว จากนั้น เชื่อมต่อโมดูล RTC ของคุณกับ Arduino ตามคู่มือการใช้งาน

นี่คือตัวอย่างง่ายๆเพื่อเริ่มต้น:

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC lost power, let's set the time!");
    // เมื่อต้องการตั้งเวลาให้กับอุปกรณ์ใหม่หรือหลังจากที่สูญเสียพลังงาน คุณสามารถตั้งเวลาได้ที่นี่
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("วันที่ปัจจุบัน: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // หน่วงเวลา 3 วินาทีเพื่อลดการรบกวนของข้อมูลจากซีเรียล
}
```

ตัวอย่างผลลัพธ์ (สมมติว่าคุณได้ตั้งเวลา RTC ของคุณก่อนหน้านี้แล้ว):

```
วันที่ปัจจุบัน: 2023/4/15
```

โค้ดนี้เริ่มต้นโมดูล RTC และจากนั้นในลูป จะดึงและพิมพ์วันที่ปัจจุบันไปยัง Serial Monitor ทุกๆ 3 วินาที จำไว้ว่า บรรทัด `rtc.adjust(...)` สามารถเอาคอมเมนต์ออกและแก้ไขเพื่อตั้งวันที่และเวลาของ RTC ใหม่หรือหลังจากที่มันสูญเสียพลังงาน
