---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:49.965111-06:00
description: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\
  \u0E01\u0E2D\u0E23\u0E4C\u0E43\u0E19 Dart \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\
  \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E42\u0E04\u0E49\
  \u0E14\u0E02\u0E2D\u0E07\u0E15\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E21\u0E35\u0E23\
  \u0E30\u0E1A\u0E1A\u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E08\
  \u0E38\u0E14\u0E2B\u0E22\u0E38\u0E14 (breakpoints), \u0E01\u0E49\u0E32\u0E27\u0E1C\
  \u0E48\u0E32\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19, \u0E41\u0E25\
  \u0E30\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\
  \u2026"
lastmod: '2024-03-17T21:57:55.902737-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\
  \u0E01\u0E2D\u0E23\u0E4C\u0E43\u0E19 Dart \u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\
  \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E42\u0E04\u0E49\
  \u0E14\u0E02\u0E2D\u0E07\u0E15\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E21\u0E35\u0E23\
  \u0E30\u0E1A\u0E1A\u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E08\
  \u0E38\u0E14\u0E2B\u0E22\u0E38\u0E14 (breakpoints), \u0E01\u0E49\u0E32\u0E27\u0E1C\
  \u0E48\u0E32\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19, \u0E41\u0E25\
  \u0E30\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\
  \u2026"
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การใช้ดีบักเกอร์ใน Dart ช่วยให้โปรแกรมเมอร์สามารถตรวจสอบโค้ดของตนอย่างมีระบบโดยการตั้งจุดหยุด (breakpoints), ก้าวผ่านการทำงาน, และตรวจสอบตัวแปร เป็นกระบวนการที่สำคัญสำหรับการระบุและแก้ไขข้อบกพร่องอย่างมีประสิทธิภาพ ทำให้เป็นเครื่องมือที่ขาดไม่ได้ในวงจรการพัฒนา

## วิธีการ:

### การดีบักพื้นฐาน:

**1. การตั้งจุดหยุด:**

เพื่อตั้งจุดหยุด เพียงคลิกที่ขอบซ้ายของบรรทัดโค้ดใน IDE ของคุณ (เช่น Visual Studio Code หรือ Android Studio) ที่คุณต้องการให้การทำงานหยุดชั่วคราว

```dart
void main() {
  var message = 'Hello, Debugging';
  print(message); // ตั้งจุดหยุดที่นี่
}
```

**2. เริ่มการดีบัก:**

ใน IDE ของคุณ ให้เริ่มการดีบักโดยการคลิกที่ไอคอนดีบักหรือกดปุ่มดีบัก การทำงานจะหยุดที่จุดหยุด

**3. ตรวจสอบตัวแปร:**

เมื่อการทำงานหยุดชั่วคราว ให้วางเมาส์เหนือตัวแปรเพื่อดูค่าปัจจุบันของมัน

**4. ก้าวผ่านโค้ด:**

ใช้คำสั่งก้าวไปข้างหน้า ก้าวเข้าไป และก้าวออกใน IDE ของคุณเพื่อเดินทางผ่านโค้ดของคุณทีละบรรทัดหรือฟังก์ชัน

### การดีบักขั้นสูงด้วย Observatory:

Dart ประกอบด้วยเครื่องมือที่เรียกว่า Observatory สำหรับการดีบักและการวิเคราะห์ประสิทธิภาพของแอปพลิเคชัน Dart มีประโยชน์โดยเฉพาะอย่างยิ่งสำหรับแอปพลิเคชันที่ทำงานบน Dart VM

**การเข้าถึง Observatory:**

รันแอปพลิเคชัน Dart ของคุณด้วยธง `--observe`

```bash
dart --observe your_program.dart
```

คำสั่งนี้จะพิมพ์ URL ออกมาที่คอนโซล ซึ่งคุณสามารถเปิดในเว็บเบราว์เซอร์เพื่อเข้าถึงดีบักเกอร์ Observatory

### การใช้ไลบรารีของบุคคลที่สามที่นิยม:

สำหรับการดีบักแอปพลิเคชัน Flutter, แพ็กเกจ `flutter_devtools` ให้ชุดเครื่องมือสำหรับการวิเคราะห์ประสิทธิภาพและการดีบักที่ทำงานร่วมกับทั้ง Dart VM และ Flutter

**การติดตั้ง:**

เพิ่ม `devtools` ลงในไฟล์ `pubspec.yaml` ของคุณภายใต้ `dev_dependencies`:

```yaml
dev_dependencies:
  devtools: any
```

**การเริ่มต้น DevTools:**

รันคำสั่งนี้ในเทอร์มินัลของคุณ:

```bash
flutter pub global run devtools
```

จากนั้น เริ่มแอปพลิเคชัน Flutter ของคุณในโหมดดีบัก DevTools ให้คุณสมบัติเช่น Flutter inspector สำหรับการวิเคราะห์ต้นไม้วิดเจ็ต และเครือข่าย profiler สำหรับการตรวจสอบกิจกรรมเครือข่าย

### ตัวอย่างผลลัพธ์:

เมื่อพบจุดหยุด โปรแกรม IDE ของคุณอาจแสดงค่าตัวแปรและสแต็คเทรซเช่นนี้:

```
message: 'Hello, Debugging'
```

โดยการใช้เครื่องมือและเทคนิกการดีบักใน Dart อย่างมีประสิทธิภาพ นักพัฒนาสามารถระบุและแก้ไขปัญหาได้อย่างรวดเร็ว นำไปสู่กระบวนการพัฒนาที่ราบรื่นขึ้นและแอปพลิเคชันที่แข็งแกร่งยิ่งขึ้น