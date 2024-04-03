---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:03.183473-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E2B\u0E49\u0E2D\u0E07\u0E2A\
  \u0E21\u0E38\u0E14\u0E2B\u0E25\u0E31\u0E01\u0E02\u0E2D\u0E07 Dart \u0E23\u0E27\u0E21\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E2A\u0E38\u0E48\u0E21\u0E14\u0E49\u0E27\u0E22\
  \u0E04\u0E25\u0E32\u0E2A `Random` \u0E17\u0E35\u0E48\u0E1E\u0E1A\u0E43\u0E19 `dart:math`\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19."
lastmod: '2024-03-17T21:57:55.891827-06:00'
model: gpt-4-0125-preview
summary: "\u0E2B\u0E49\u0E2D\u0E07\u0E2A\u0E21\u0E38\u0E14\u0E2B\u0E25\u0E31\u0E01\
  \u0E02\u0E2D\u0E07 Dart \u0E23\u0E27\u0E21\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2A\
  \u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\
  \u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E2A\
  \u0E38\u0E48\u0E21\u0E14\u0E49\u0E27\u0E22\u0E04\u0E25\u0E32\u0E2A `Random` \u0E17\
  \u0E35\u0E48\u0E1E\u0E1A\u0E43\u0E19 `dart:math` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\
  \u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\
  \u0E19."
title: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\u0E25\
  \u0E02\u0E2A\u0E38\u0E48\u0E21"
weight: 12
---

## วิธีทำ:
ห้องสมุดหลักของ Dart รวมถึงการสนับสนุนสำหรับการสร้างตัวเลขสุ่มด้วยคลาส `Random` ที่พบใน `dart:math` นี่คือตัวอย่างพื้นฐาน:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // สร้างตัวเลขเต็มสุ่มระหว่าง 0 ถึง 99
  double randomDouble = rand.nextDouble(); // สร้างตัวเลขทศนิยมสุ่มระหว่าง 0.0 ถึง 1.0
  print(randomNumber);
  print(randomDouble);
}
```

*ผลลัพธ์ตัวอย่าง: (จะแตกต่างกันทุกครั้งที่ทำการรัน)*

```
23
0.6722390975465775
```

สำหรับกรณีการใช้งานที่ต้องการความไม่คาดเดาในแง่ของเข้ารหัสลับ, Dart เสนอตัวสร้าง `Random.secure`:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*ผลลัพธ์ตัวอย่าง: (จะแตกต่างกันทุกครั้งที่ทำการรัน)*

```
45
```

หากคุณกำลังทำงานกับโปรเจ็กต์ Flutter หรือต้องการความสุ่มที่ซับซ้อนมากขึ้น คุณอาจพบว่าแพ็คเกจ `faker` มีประโยชน์ในการสร้างข้อมูลสุ่มหลากหลายชนิด เช่น ชื่อ ที่อยู่ และวันที่

เพื่อใช้ `faker`, ก่อนอื่น ให้เพิ่มเข้าไปในไฟล์ `pubspec.yaml` ของคุณ:

```yaml
dependencies:
  faker: ^2.0.0
```

จากนั้น นำเข้าและใช้งานดังแสดง:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // สร้างชื่อสุ่ม
  print(faker.address.city()); // สร้างชื่อเมืองสุ่ม
}
```

*ผลลัพธ์ตัวอย่าง:*

```
Josie Runolfsdottir
East Lysanne
```
