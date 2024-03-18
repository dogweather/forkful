---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:56.316322-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E43\u0E19 Google\
  \ Apps Script (GAS) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E23\u0E30\
  \u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E41\
  \u0E25\u0E30\u0E25\u0E1A\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\
  \u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E17\
  \u0E35\u0E48\u0E21\u0E35\u0E08\u0E38\u0E14\u0E1B\u0E23\u0E30\u0E2A\u0E07\u0E04\u0E4C\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2D\u0E2D\u0E42\u0E15\u0E40\u0E21\
  \u0E15 Google Apps \u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E40\u0E27\
  \u0E47\u0E1A\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\u2026"
lastmod: '2024-03-17T21:57:55.722856-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E43\u0E19 Google Apps\
  \ Script (GAS) \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E23\u0E30\u0E1A\
  \u0E27\u0E19\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E41\u0E25\
  \u0E30\u0E25\u0E1A\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E17\u0E35\
  \u0E48\u0E21\u0E35\u0E08\u0E38\u0E14\u0E1B\u0E23\u0E30\u0E2A\u0E07\u0E04\u0E4C\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2D\u0E2D\u0E42\u0E15\u0E40\u0E21\u0E15\
  \ Google Apps \u0E2B\u0E23\u0E37\u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E40\u0E27\
  \u0E47\u0E1A\u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การดีบักใน Google Apps Script (GAS) หมายถึงกระบวนการตรวจสอบและลบข้อผิดพลาดออกจากสคริปต์ที่มีจุดประสงค์เพื่อการออโตเมต Google Apps หรือสร้างเว็บแอปพลิเคชัน โปรแกรมเมอร์ดีบักเพื่อให้แน่ใจว่าโค้ดทำงานอย่างที่คาดหวัง เพิ่มความน่าเชื่อถือและประสิทธิภาพในแอปพลิเคชัน

## วิธีการ:

Google Apps Script มีดีบักเกอร์ในตัวภายใน Apps Script Editor เพื่อช่วยในการแก้ไขปัญหาสคริปต์ นี่คือวิธีที่จะเริ่มต้นและใช้งานดีบักเกอร์:

1. **เปิดสคริปต์ของคุณใน Apps Script Editor**
2. **เลือกฟังก์ชันที่จะดีบัก** จากเมนูแบบดรอปดาวน์ที่ด้านบน ให้เลือกฟังก์ชันที่คุณต้องการดีบัก
3. **ตั้งจุดหยุด (breakpoints)** คลิกที่ gutter (พื้นที่สีเทาทางด้านซ้ายของตัวเลขบรรทัด) ที่คุณต้องการหยุดการทำงาน; จุดสีแดงจะปรากฏขึ้น บ่งบอกถึงจุดหยุด
4. **เริ่มการดีบัก** คลิกที่ไอคอนแมลงหรือเลือก `ดีบัก` > `เริ่มการดีบัก` การทำงานจะเริ่มและหยุดที่จุดหยุดแรก

พิจารณาสคริปต์ง่ายๆนี้:

```javascript
function calculateSum() {
  var a = 5;
  var b = 10;
  var sum = a + b;
  Logger.log(sum); // มีจุดประสงค์ที่จะล็อก 15
}
```

หากไม่แน่ใจว่าทำไม `Logger.log(sum)` ไม่แสดงผลลัพธ์ที่คาดหวัง คุณสามารถตั้งจุดหยุดที่บรรทัด `var sum = a + b;` และไล่ผ่านสคริปต์ทีละบรรทัดเพื่อตรวจสอบค่าตัวแปร

**ตัวอย่างผลลัพธ์ใน Logger:**

```plain
15
```

ระหว่างการดีบัก Apps Script Editor อนุญาตให้คุณ:

- **เดินผ่านโค้ด** โดยใช้ปุ่ม step over, step into และ step out
- **ติดตามการแสดงค่าตามเวลาจริงของ expressions และ variables**
- **ตรวจสอบ call stack** เพื่อติดตามการเรียกใช้ฟังก์ชัน

## การศึกษาลึก

การดีบักใน Google Apps Script, เหมือนในสภาพแวดล้อมการเขียนโปรแกรมอื่นๆ, มีความจำเป็นสำหรับการสร้างแอปพลิเคชันที่ปราศจากข้อผิดพลาด ดีบักเกอร์ในตัวที่ถูกนำมาใช้ในตอนต้นของการพัฒนา GAS นำเสนอความสามารถพื้นฐานในการตรวจสอบและแก้ไขโค้ดอย่างค่อยเป็นค่อยไป แม้ว่าจะมีคุณสมบัติการดีบักพื้นฐานที่คล้ายคลึงกับสภาพแวดล้อมที่เจริญมากขึ้น เช่น Visual Studio Code หรือ IntelliJ แต่อาจจะไม่เพียงพอสำหรับสถานการณ์การดีบักที่ซับซ้อน เช่น ความสามารถในการตรวจสอบการเรียกใช้งานอย่างเรียกกลับแบบอะซิงโครนัสหรือการจัดการการดำเนินการสคริปต์ขนาดใหญ่อาจจะมีข้อจำกัด

สำหรับความต้องการการดีบักที่ซับซ้อน นักพัฒนาอาจจะหันไปใช้วิธีการอื่น เช่น การล็อคอย่างกว้างขวาง (โดยใช้ `Logger.log()`) หรือแม้กระทั่งการปรับใช้เป็นเว็บแอปเพื่อตรวจสอบพฤติกรรมในสถานการณ์จริง อย่างไรก็ตาม ความง่ายและการบูรณาการของดีบักเกอร์ GAS ภายใน Apps Script Editor ทำให้เป็นขั้นตอนแรกที่มีค่าไม่แพ้กันสำหรับการแก้ไขปัญหาและเข้าใจพฤติกรรมของสคริปต์ โดยเฉพาะอย่างยิ่ง ด้วยการอัปเดตและการปรับปรุงของ Google ต่อ Apps Script อย่างต่อเนื่อง ประสบการณ์การดีบักก็กำลังดีขึ้นอย่างต่อเนื่อง นำเสนอเครื่องมือและตัวเลือกที่มีความซับซ้อนมากขึ้นตามเวลา ความก้าวหน้านี้สะท้อนถึงความมุ่งมั่นของ Google ในการทำให้ Apps Script เป็นแพลตฟอร์มที่ทรงพลังและเข้าถึงได้มากขึ้นสำหรับนักพัฒนาจากหลากหลายพื้นหลัง