---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:15.522834-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: Arduino \u0E44\u0E21\u0E48\u0E21\
  \u0E35\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 logging \u0E43\u0E19\u0E15\u0E31\
  \u0E27\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E2A\u0E20\u0E32\u0E1E\
  \u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E2D\u0E37\u0E48\u0E19\u0E46 \u0E41\u0E15\
  \u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E01\u0E32\
  \u0E23 logging \u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E44\u0E1B\u0E22\u0E31\
  \u0E07 Serial console \u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E22\
  \u0E38\u0E48\u0E07\u0E22\u0E32\u0E01\u2026"
lastmod: '2024-03-17T21:57:56.487751-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u0E44\u0E21\u0E48\u0E21\u0E35\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\
  \u0E35 logging \u0E43\u0E19\u0E15\u0E31\u0E27\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\
  \u0E01\u0E31\u0E1A\u0E2A\u0E20\u0E32\u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\
  \u0E2D\u0E37\u0E48\u0E19\u0E46 \u0E41\u0E15\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E17\u0E33\u0E01\u0E32\u0E23 logging \u0E1E\u0E37\u0E49\u0E19\
  \u0E10\u0E32\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07 Serial console \u0E44\u0E14\u0E49\
  \u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E22\u0E38\u0E48\u0E07\u0E22\u0E32\u0E01 \u0E19\
  \u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\
  \u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E2B\
  \u0E49\u0E04\u0E38\u0E13\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E44\u0E14\
  \u0E49."
title: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E25\u0E47\u0E2D\u0E01"
weight: 17
---

## วิธีทำ:
Arduino ไม่มีไลบรารี logging ในตัวเหมือนกับสภาพแวดล้อมอื่นๆ แต่คุณสามารถทำการ logging พื้นฐานไปยัง Serial console ได้โดยไม่ยุ่งยาก นี่คือตัวอย่างเริ่มต้นเพื่อให้คุณเริ่มต้นได้:

```arduino
void setup() {
  // เริ่มการสื่อสารผ่าน serial ด้วยอัตราบอดที่กำหนด
  Serial.begin(9600);

  // รอจนกว่า serial port จะเชื่อมต่อ - จำเป็นเฉพาะบนบางบอร์ด
  while (!Serial) {
    ; // รอการเชื่อมต่อของพอร์ต serial จำเป็นสำหรับ USB แบบเนทีฟ
  }

  // Log ข้อความข้อมูลที่บอกว่ากระบวนการตั้งค่าเสร็จสมบูรณ์
  Serial.println("การตั้งค่าเสร็จสิ้น!");
}

void loop() {
  // Logger ง่ายๆ ที่พิมพ์เวลาทำงานทุกวินาที
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("เวลาทำงาน (มิลลิวินาที): ");
    Serial.println(currentMillis);

    // ที่นี่คุณอาจเพิ่ม error logs, warnings, หรือข้อมูลอื่นๆ.
  }
  
  // ตรรกะโปรแกรมอื่นๆของคุณที่นี่...
}
```

ตัวอย่าง Serial Output:
```
การตั้งค่าเสร็จสิ้น!
เวลาทำงาน (มิลลิวินาที): 1000
เวลาทำงาน (มิลลิวินาที): 2000
เวลาทำงาน (มิลลิวินาที): 3000
...
```

## ภาพรวมลึก:
ในอดีต, logging บนไมโครคอนโทรลเลอร์ไม่ง่ายเหมือนบนระบบปฏิบัติการที่แคบอนหน่อเท่าไร ทรัพยากรที่จำกัดหมายความว่าทุกไบต์มีความสำคัญ และนักพัฒนาต้องระมัดระวังไม่ให้ระบบถูกอุดตัน ด้วยการมาถึงของบอร์ดที่มีความสามารถมากขึ้นและแพลตฟอร์ม Arduino ทำให้กระบวนการง่ายขึ้น, logging จึงกลายเป็นเรื่องที่เข้าถึงได้มากขึ้น

ในขณะที่โค้ดข้างต้นแสดงการ logging ผ่าน Serial interface วิธีอื่น ๆ รวมถึงการเขียนไปยังการ์ด SD, การส่งข้อมูลผ่านเครือข่ายไปยังเซิร์ฟเวอร์ห่างไกล, หรือแม้แต่การแสดงผลไปยัง LCD ขนาดเล็ก

การใช้งานระบบ logging นำเสนอความพิจารณาเช่นการหมุนเวียน, ความรุนแรงของระดับ (info, debug, warning, error), และผลกระทบต่อประสิทธิภาพ บน Arduino, คุณอาจต้องระมัดระวังเรื่องข้อจำกัดของหน่วยความจำเมื่อ logging โครงสร้างข้อมูลที่ซับซ้อน สำหรับการ logging จากระยะไกล, ความปลอดภัยของแฟ้มบันทึกที่ถูกส่งก็เป็นประเด็นที่ต้องกังวล

โซลูชันที่ซับซ้อนยิ่งขึ้นเช่น Syslog, มาตรฐานการบันทึกที่ได้รับการยอมรับอย่างกว้างขวาง, มีอยู่ภายนอกโลกของ Arduino, แต่คุณสามารถรวมไลบรารีของบุคคลที่สามที่เสนอความสามารถคล้ายคลึงกันด้วยระดับความซับซ้อนและความต้องการทรัพยากรที่แตกต่างกันได้

## ดูเพิ่มเติม:
- [อ้างอิงของ `Serial` ของ Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [การบันทึกข้อมูลด้วยการ์ด SD กับ Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [Data Logging shield ของ SparkFun](https://www.sparkfun.com/products/13712)
- [TinyWeb: ตัวอย่างการบันทึกข้อมูลจากระยะไกลกับ Arduino](https://www.arduino.cc/en/Tutorial/WebClientRepeating)
