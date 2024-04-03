---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:24.168026-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E08\u0E34\u0E19\u0E15\
  \u0E19\u0E32\u0E01\u0E32\u0E23\u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\u0E15\u0E49\u0E2D\
  \u0E07\u0E01\u0E32\u0E23\u0E43\u0E2B\u0E49 LED \u0E01\u0E23\u0E30\u0E1E\u0E23\u0E34\
  \u0E1A \u0E2B\u0E32\u0E01\u0E44\u0E21\u0E48\u0E21\u0E35\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E19 \u0E25\u0E39\u0E1B\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E08\u0E30\
  \u0E40\u0E1B\u0E47\u0E19\u0E01\u0E2D\u0E07\u0E23\u0E30\u0E40\u0E23\u0E37\u0E48\u0E2D\
  \u0E22\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E41\u0E19\u0E48\u0E19\u0E2D\u0E19 \u0E41\u0E15\
  \u0E48\u0E14\u0E49\u0E27\u0E22\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 \u0E21\
  \u0E31\u0E19\u0E08\u0E30\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\
  \u0E1A \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\
  ."
lastmod: '2024-03-17T21:57:56.486796-06:00'
model: gpt-4-0125-preview
summary: "\u0E08\u0E34\u0E19\u0E15\u0E19\u0E32\u0E01\u0E32\u0E23\u0E27\u0E48\u0E32\
  \u0E04\u0E38\u0E13\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E43\u0E2B\u0E49 LED\
  \ \u0E01\u0E23\u0E30\u0E1E\u0E23\u0E34\u0E1A \u0E2B\u0E32\u0E01\u0E44\u0E21\u0E48\
  \u0E21\u0E35\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 \u0E25\u0E39\u0E1B\u0E02\
  \u0E2D\u0E07\u0E04\u0E38\u0E13\u0E08\u0E30\u0E40\u0E1B\u0E47\u0E19\u0E01\u0E2D\u0E07\
  \u0E23\u0E30\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E22\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E41\
  \u0E19\u0E48\u0E19\u0E2D\u0E19 \u0E41\u0E15\u0E48\u0E14\u0E49\u0E27\u0E22\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 \u0E21\u0E31\u0E19\u0E08\u0E30\u0E40\u0E1B\u0E47\
  \u0E19\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\
  \u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A\
  \u0E42\u0E04\u0E49\u0E14\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19"
weight: 18
---

## วิธีการ:
จินตนาการว่าคุณต้องการให้ LED กระพริบ หากไม่มีฟังก์ชัน ลูปของคุณจะเป็นกองระเรื่อยอย่างแน่นอน แต่ด้วยฟังก์ชัน มันจะเป็นระเบียบ นี่คือวิธีการ:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // กระพริบ LED ทุก ๆ 500 มิลลิวินาที
}

// ฟังก์ชันสำหรับการกระพริบ LED
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

ผลลัพธ์ตัวอย่าง: LED ของคุณกำลังกระพริบอย่างมีความสุข และจุดประสงค์ของโค้ดสามารถเห็นได้ชัดเจนจากการมองเพียงแวบเดียว

## ลึกซึ้งยิ่งขึ้น
ก่อนมีฟังก์ชัน การเขียนโปรแกรมคือการเดินทางบนถนนเส้นตรง คุณเจอทุกหลุมบนถนนตั้งแต่ต้นจนจบ หลังจากมีฟังก์ชัน มันเหมือนกับการกระโดดขึ้นเครื่องบิน - คุณสามารถข้ามไปยังส่วนที่สำคัญได้ อย่างประวัติศาสตร์แล้ว ซับรูทีน (ฟังก์ชันในยุคแรก ๆ) ถือเป็นการปฏิวัติในวงการโปรแกรมมิ่ง ช่วยให้โค้ดเดอร์หลีกเลี่ยงการทำซ้ำ – นั่นคือหลักการ DRY หรือ Don’t Repeat Yourself ตัวเลือกที่ไม่ใช่ฟังก์ชันอาจรวมถึงมาโคร หรือการใช้คลาสสำหรับการเขียนโปรแกรมเชิงวัตถุ (OOP) ประเด็นสำคัญ? เมื่อคุณกำหนดฟังก์ชัน คุณกำลังให้คอมไพเลอร์มีแบบแผนสำหรับการดำเนินการงาน กับ Arduino คุณมักจะกำหนดฟังก์ชัน void ที่ทำหน้าที่เป็นคำสั่งง่าย ๆ สำหรับไมโครคอนโทรลเลอร์ แต่ฟังก์ชันก็สามารถส่งค่ากลับได้ ทำให้มีความหลากหลายมากขึ้น

## ดูเพิ่มเติม
สำหรับข้อมูลเพิ่มเติมเกี่ยวกับฟังก์ชัน สามารถเรียกดูผ่านทางเหล่านี้:

- อ้างอิงฟังก์ชันอย่างเป็นทางการของ Arduino: https://www.arduino.cc/reference/en/language/functions/
- เรียนรู้เพิ่มเติมเกี่ยวกับหลักการ DRY: https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
- การทบทวนประวัติศาสตร์ของซับรูทีน: https://en.wikipedia.org/wiki/Subroutine
