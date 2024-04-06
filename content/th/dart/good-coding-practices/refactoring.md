---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:15.916666-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E48\u0E2D\u0E19\
  \u0E01\u0E32\u0E23 refactor, \u0E04\u0E38\u0E13\u0E2D\u0E32\u0E08\u0E21\u0E35\u0E42\
  \u0E04\u0E49\u0E14\u0E17\u0E35\u0E48\u0E1C\u0E2A\u0E21\u0E1C\u0E2A\u0E32\u0E19\u0E23\
  \u0E30\u0E14\u0E31\u0E1A\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\
  \u0E04\u0E27\u0E32\u0E21\u0E04\u0E34\u0E14\u0E2B\u0E23\u0E37\u0E2D\u0E04\u0E27\u0E32\
  \u0E21\u0E23\u0E31\u0E1A\u0E1C\u0E34\u0E14\u0E0A\u0E2D\u0E1A\u0E15\u0E48\u0E32\u0E07\
  \u0E46 \u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\
  \u0E2A\u0E48\u0E27\u0E19\u0E25\u0E14\u0E41\u0E25\u0E49\u0E27\u0E08\u0E36\u0E07\u0E43\
  \u0E0A\u0E49\u0E21\u0E31\u0E19."
lastmod: '2024-03-17T21:57:55.907345-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
weight: 19
---

## วิธีการ:


### ตัวอย่างที่ 1: การเปลี่ยนชื่อและการสกัดวิธีการ
ก่อนการ refactor, คุณอาจมีโค้ดที่ผสมผสานระดับของการแสดงความคิดหรือความรับผิดชอบต่างๆ เช่น การคำนวณส่วนลดแล้วจึงใช้มัน:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("Final price: $finalPrice");
}
```

**ผลลัพธ์:**
```
Final price: 80.0
```

หลังจากการ refactor, คุณสามารถสกัดการคำนวณส่วนลดออกมาเป็นวิธีการของตัวมันเองและตั้งชื่อที่มีความหมาย:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = calculateFinalPrice(price, discount);
  print("Final price: $finalPrice");
}

double calculateFinalPrice(double price, double discount) {
  return price - (price * discount);
}
```

**ผลลัพธ์:**
```
Final price: 80.0
```

โดยการสกัดการคำนวณออกมาเป็นวิธีการ คุณจะมีการดำเนินงานที่ได้รับการกำหนดอย่างชัดเจนซึ่งสามารถนำไปใช้ซ้ำ, ทดสอบได้อย่างอิสระ และมีการแก้ไขได้ง่าย

### ตัวอย่างที่ 2: การทำให้เงื่อนไขง่ายขึ้น
ก่อนการ refactor, คำสั่งเงื่อนไขอาจซับซ้อนหรือยากต่อการอ่าน:

```dart
void main() {
  var customerType = "regular";
  double discount;
  
  if (customerType == "regular") {
    discount = 0.05;
  } else if (customerType == "member") {
    discount = 0.1;
  } else {
    discount = 0.0;
  }

  print("Discount: $discount");
}
```

**ผลลัพธ์:**
```
Discount: 0.05
```

หลังจากการ refactor, พิจารณาใช้แมพเพื่อโครงสร้างที่ชัดเจนและการอัปเดตหรือขยายประเภทของลูกค้าและส่วนลดง่ายขึ้น:

```dart
void main() {
  var customerType = "regular";
  var discounts = {
    "regular": 0.05,
    "member": 0.1,
    "none": 0.0,
  };

  var discount = discounts[customerType] ?? 0.0;
  print("Discount: $discount");
}
```

**ผลลัพธ์:**
```
Discount: 0.05
```

การ refactor นี้ไม่เพียงทำให้โค้ดกระชับมากขึ้น แต่ยังรวมเอาตรรกะสำหรับการกำหนดส่วนลดในวิธีที่เข้าใจและบำรุงรักษาได้ง่ายขึ้น

### ไลบรารีของบุคคลที่สามสำหรับการ Refactor
เมื่อต้องการ refactor ใน Dart, โดยเฉพาะในแอป Flutter, [Dart DevTools](https://dart.dev/tools/dart-devtools) นั้นมีค่ายิ่ง ซึ่งรวมถึงเครื่องมือด้านประสิทธิภาพ, ตรวจสอบวิดเจ็ต, และดีบักเกอร์ระดับซอร์สโค้ด แม้ว่า Dart DevTools จะไม่ใช่ไลบรารีของบุคคลที่สาม, มักจะใช้ควบคู่ไปกับไลบรารีเช่น `flutter_bloc` เพื่อการจัดการสถานะอย่างเรียบร้อยที่เอื้ออำนวยต่อการ refactor เพื่อปรับปรุงความโมดูลาร์และการอ่านได้ง่าย โชคร้ายที่ขอบเขตของบทความนี้ ตัวอย่างโค้ดโดยใช้ไลบรารีของบุคคลที่สามจะไม่ถูกนำเสนอที่นี่ แต่นักพัฒนาจูงใจให้สำรวจเครื่องมือเหล่านี้เพื่อเพิ่มการปรับปรุงกระบวนการ refactor ในแอป Dart/Flutter ของตน
