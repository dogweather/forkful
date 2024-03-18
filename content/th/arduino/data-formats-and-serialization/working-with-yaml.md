---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:36.745387-06:00
description: "YAML (YAML Ain't Markup Language) \u0E40\u0E1B\u0E47\u0E19\u0E21\u0E32\
  \u0E15\u0E23\u0E10\u0E32\u0E19\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\
  \u0E44\u0E25\u0E0B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\
  \u0E48\u0E32\u0E19\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\u0E22\
  \u0E4C\u0E0B\u0E36\u0E48\u0E07\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E15\
  \u0E31\u0E49\u0E07\u0E04\u0E48\u0E32, \u0E01\u0E32\u0E23\u0E2A\u0E37\u0E48\u0E2D\
  \u0E2A\u0E32\u0E23\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\u0E07\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21,\u2026"
lastmod: '2024-03-17T21:57:56.501201-06:00'
model: gpt-4-0125-preview
summary: "YAML (YAML Ain't Markup Language) \u0E40\u0E1B\u0E47\u0E19\u0E21\u0E32\u0E15\
  \u0E23\u0E10\u0E32\u0E19\u0E01\u0E32\u0E23\u0E0B\u0E35\u0E40\u0E23\u0E35\u0E22\u0E44\
  \u0E25\u0E0B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E2D\u0E48\
  \u0E32\u0E19\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C\
  \u0E0B\u0E36\u0E48\u0E07\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C\u0E01\u0E32\u0E23\u0E15\u0E31\
  \u0E49\u0E07\u0E04\u0E48\u0E32, \u0E01\u0E32\u0E23\u0E2A\u0E37\u0E48\u0E2D\u0E2A\
  \u0E32\u0E23\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\u0E07\u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21,\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

YAML (YAML Ain't Markup Language) เป็นมาตรฐานการซีเรียไลซ์ข้อมูลที่อ่านได้โดยมนุษย์ซึ่งสามารถใช้สำหรับไฟล์การตั้งค่า, การสื่อสารระหว่างโปรแกรม, และการจัดเก็บข้อมูล โปรแกรมเมอร์หันมาใช้ YAML สำหรับโปรเจค Arduino เพื่อทำให้กระบวนการตั้งค่าของแอปพลิเคชันง่ายขึ้น ทำให้สามารถแก้ไขพารามิเตอร์ได้โดยไม่ต้องดำดิ่งลงไปในโค้ดอย่างลึกซึ้ง, เพิ่มความสามารถในการอ่าน, และทำให้การแชร์การตั้งค่าง่ายขึ้น

## วิธีการ:

การทำงานกับ YAML โดยตรงบน Arduino ไม่ง่ายเหมือนในสภาพแวดล้อมการเขียนโปรแกรมระดับสูง เนื่องจากข้อจำกัดของหน่วยความจำและไม่มีไลบรารีการประมวลผล YAML เนทีฟ อย่างไรก็ตาม สำหรับโปรเจคที่ต้องการการแยกวิเคราะห์หรือการสร้าง YAML วิธีการทั่วไปคือการใช้คอมพิวเตอร์พาหนะ (เช่น Raspberry Pi) หรือการแปลงไฟล์ YAML เป็นรูปแบบที่เหมาะกับ Arduino มากขึ้น (เช่น JSON) โดยใช้สคริปต์ภายนอก เพื่อจุดประสงค์ในการสาธิต เราจะเน้นที่วิธีการหลังโดยใช้ไลบรารีที่ได้รับความนิยม: ArduinoJson

**ขั้นตอนที่ 1:** แปลงการตั้งค่า YAML ของคุณเป็น JSON คุณสามารถใช้เครื่องมือออนไลน์หรือยูทิลิตี้บรรทัดคำสั่งเช่น `yq`

ไฟล์ YAML (`config.yaml`):
```yaml
wifi:
  ssid: "YourSSID"
  password: "YourPassword"
```

แปลงเป็น JSON (`config.json`):
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "password": "YourPassword"
  }
}
```

**ขั้นตอนที่ 2:** ใช้ไลบรารี ArduinoJson เพื่อวิเคราะห์ไฟล์ JSON ในสเกตช์ Arduino ของคุณ ก่อนอื่น คุณต้องติดตั้งไลบรารี ArduinoJson ผ่าน Library Manager ใน Arduino IDE

**ขั้นตอนที่ 3:** โหลดและวิเคราะห์ JSON ในโค้ดของคุณ เนื่องจากข้อจำกัดด้านพื้นที่จัดเก็บของ Arduino จินตนาการว่าสตริง JSON ถูกจัดเก็บในตัวแปรหรืออ่านจากการ์ด SD

ตัวอย่างสเกตช์ Arduino:
```cpp
#include <ArduinoJson.h>

const char* jsonConfig = "{\"wifi\":{\"ssid\":\"YourSSID\",\"password\":\"YourPassword\"}}";

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, jsonConfig);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  const char* ssid = doc["wifi"]["ssid"]; // "YourSSID"
  const char* password = doc["wifi"]["password"]; // "YourPassword"

  Serial.print("SSID: ");
  Serial.println(ssid);
  Serial.print("Password: ");
  Serial.println(password);
}

void loop() {
  // ไม่มีอะไรที่นี่สำหรับตัวอย่างนี้
}
```

ผลลัพธ์เมื่อทำการรันสเกตช์:
```
SSID: YourSSID
Password: YourPassword
```

วิธีการนี้ที่เกี่ยวข้องกับการแปลงเป็น JSON และการใช้ไลบรารี ArduinoJson ทำให้สามารถจัดการการตั้งค่า YAML ภายในโปรเจค Arduino ได้อย่างง่ายดาย หลีกเลี่ยงการวิเคราะห์ YAML โดยตรงบนไมโครคอนโทรลเลอร์
