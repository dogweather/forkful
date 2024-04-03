---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:10.301036-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Dart, \u0E41\
  \u0E1E\u0E04\u0E40\u0E01\u0E08 `test` \u0E16\u0E39\u0E01\u0E43\u0E0A\u0E49\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\
  \u0E14\u0E2A\u0E2D\u0E1A\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E17\
  \u0E35\u0E48\u0E19\u0E34\u0E22\u0E21 \u0E01\u0E48\u0E2D\u0E19\u0E2D\u0E37\u0E48\u0E19\
  , \u0E40\u0E1E\u0E34\u0E48\u0E21\u0E41\u0E1E\u0E04\u0E40\u0E01\u0E08 `test` \u0E25\
  \u0E07\u0E43\u0E19 `pubspec.yaml` \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13."
lastmod: '2024-03-17T21:57:55.901090-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Dart, \u0E41\u0E1E\u0E04\u0E40\u0E01\u0E08 `test` \u0E16\u0E39\
  \u0E01\u0E43\u0E0A\u0E49\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\
  \u0E02\u0E35\u0E22\u0E19\u0E17\u0E14\u0E2A\u0E2D\u0E1A\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E40\u0E1B\u0E47\u0E19\u0E17\u0E35\u0E48\u0E19\u0E34\u0E22\u0E21 \u0E01\u0E48\u0E2D\
  \u0E19\u0E2D\u0E37\u0E48\u0E19, \u0E40\u0E1E\u0E34\u0E48\u0E21\u0E41\u0E1E\u0E04\
  \u0E40\u0E01\u0E08 `test` \u0E25\u0E07\u0E43\u0E19 `pubspec.yaml` \u0E02\u0E2D\u0E07\
  \u0E04\u0E38\u0E13."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E01\u0E32\u0E23\u0E17\u0E14\
  \u0E2A\u0E2D\u0E1A"
weight: 36
---

## วิธีการ:
ใน Dart, แพคเกจ `test` ถูกใช้สำหรับการเขียนทดสอบอย่างเป็นที่นิยม ก่อนอื่น, เพิ่มแพคเกจ `test` ลงใน `pubspec.yaml` ของคุณ:

```yaml
dev_dependencies:
  test: ^1.0.0
```

จากนั้น, เขียนทดสอบสำหรับฟังก์ชั่นง่ายๆ สมมุติว่าคุณมีฟังก์ชั่นที่บวกเลขสองตัว:

```dart
int add(int a, int b) {
  return a + b;
}
```

ต่อไป, สร้างไฟล์ชื่อ `add_test.dart` ในไดเรกทอรี `test` และเขียนกรณีทดสอบของคุณ:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // สมมุติว่าฟังก์ชั่น `add` ของคุณอยู่ใน lib/add.dart

void main() {
  test('บวกเลขสองตัว', () {
    var expected = 3;
    expect(add(1, 2), equals(expected));
  });
}
```

เพื่อรันการทดสอบ, ใช้คำสั่ง Dart:

```bash
$ dart test
```

ตัวอย่างผลลัพธ์อาจจะเป็น:

```
00:01 +1: ผ่านทุกทดสอบ!
```

### การใช้ไลบรารีจากบุคคลที่สาม: Mockito สำหรับ mocking
สำหรับการทดสอบโค้ดที่มีการพึ่งพาอย่างซับซ้อน, คุณอาจจะใช้ Mockito เพื่อสร้างวัตถุเลียนแบบ ก่อนอื่น, เพิ่ม Mockito เข้าไปใน `pubspec.yaml` ของคุณ:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

สมมุติว่าคุณมีคลาส `UserRepository` ที่ดึงข้อมูลผู้ใช้, และคุณต้องการทดสอบ `UserService` ที่พึ่งพา `UserRepository` โดยไม่ต้องติดต่อกับฐานข้อมูลจริง:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// สร้างคลาส Mock โดยใช้ Mockito
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('UserService Tests', () {
    test('ดึงข้อมูลผู้ใช้อย่างสำเร็จ', () {
      // สร้าง instance เลียนแบบ
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // ตั้งค่าพฤติกรรมเลียนแบบ
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Test User'));

      // ยืนยันว่าเมธอดที่เลียนแบบถูกเรียกด้วยอาร์กิวเมนต์ที่คาดหวัง
      expect(userService.getUserName(1), 'Test User');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

การรันการทดสอบนี้ยืนยันว่า `UserService` สามารถทำงานร่วมกับ `UserRepository` ได้ถูกต้อง โดยใช้วิธี mocking เพื่อจำลองการโต้ตอบจริงในแบบที่ควบคุมได้.
