---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:56.367420-06:00
description: "Dart-\u098F \u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09C7\u0996\u09BE\u09B0\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09A1\u09BF\u09AB\u09BE\u09B0\u09C7\u09A8\
  \u09CD\u099F \u09AA\u09BE\u09B0\u09CD\u099F\u09B8 \u0985\u09AB \u0987\u0993\u09B0\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u09A0\u09BF\u0995\u09AE\
  \u09A4\u09CB \u0995\u09BE\u099C \u0995\u09B0\u099B\u09C7 \u0995\u09BF\u09A8\u09BE\
  \ \u09B8\u09C7\u099F\u09BE \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u099F\u09C7\u09B8\u09CD\u099F \u0995\u09C7\u09B8 \u0985\
  \u099F\u09CB\u09AE\u09C7\u099F\u09BF\u0995\u09CD\u09AF\u09BE\u09B2\u09BF \u09A4\u09C8\
  \u09B0\u09BF \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.722861-06:00'
model: gpt-4-0125-preview
summary: "Dart-\u098F \u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09C7\u0996\u09BE\u09B0\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09A1\u09BF\u09AB\u09BE\u09B0\u09C7\u09A8\
  \u09CD\u099F \u09AA\u09BE\u09B0\u09CD\u099F\u09B8 \u0985\u09AB \u0987\u0993\u09B0\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u09A0\u09BF\u0995\u09AE\
  \u09A4\u09CB \u0995\u09BE\u099C \u0995\u09B0\u099B\u09C7 \u0995\u09BF\u09A8\u09BE\
  \ \u09B8\u09C7\u099F\u09BE \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u099F\u09C7\u09B8\u09CD\u099F \u0995\u09C7\u09B8 \u0985\
  \u099F\u09CB\u09AE\u09C7\u099F\u09BF\u0995\u09CD\u09AF\u09BE\u09B2\u09BF \u09A4\u09C8\
  \u09B0\u09BF \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09BE\u09A6\u09C7\u09B0 \u0995\u09CB\
  \u09A1 \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0\u09AF\u09CB\u0997\u09CD\u09AF \u098F\
  \u09AC\u0982 \u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u09AE\u09C1\u0995\u09CD\u09A4\
  \ \u09B0\u09BE\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u0995\
  \u09B0\u09C7, \u09AF\u09BE \u0986\u09B0\u09CB \u09B8\u09B9\u099C\u09C7 \u0986\u09AA\
  \u09A1\u09C7\u099F \u098F\u09AC\u0982 \u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\
  \u09CD\u099F\u09B0\u09BF\u0982 \u0995\u09B0\u09BE\u09B0 \u09B8\u09B9\u09BE\u09DF\
  \u09A4\u09BE \u0995\u09B0\u09C7 \u098F\u09AC\u0982 \u09B0\u09BF\u0997\u09CD\u09B0\
  \u09C7\u09B6\u09A8 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B0\u09CB\u09A7 \u0995\u09B0\
  \u09C7\u0964."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কিভাবে:
Dart-এ, `test` প্যাকেজ টেস্ট লেখার জন্য সাধারণত ব্যবহৃত হয়। প্রথমে, তোমার `pubspec.yaml`-এ `test` প্যাকেজ যোগ কর:

```yaml
dev_dependencies:
  test: ^1.0.0
```

তারপর, একটি সিম্পল ফাংশনের জন্য একটি টেস্ট লিখ। ধর, তোমার কাছে একটি ফাংশন আছে যেটি দুই সংখ্যা যুক্ত করে:

```dart
int add(int a, int b) {
  return a + b;
}
```

এরপর, `test` ডিরেক্টরিতে `add_test.dart` নামে একটি ফাইল তৈরি কর এবং তোমার টেস্ট কেস লিখ:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // ধরে নাও তোমার `add` ফাংশনটি lib/add.dart এ আছে

void main() {
  test('adds two numbers', () {
    var expected = 3;
    expect(add(1, 2), equals(expected));
  });
}
```

টেস্টগুলি রান করতে, Dart কমান্ড ব্যবহার কর:

```bash
$ dart test
```

সেম্পল আউটপুট হতে পারে:

```
00:01 +1: All tests passed!
```

### থার্ড-পার্টি লাইব্রেরি ব্যবহার: মকিংয়ের জন্য Mockito
যদি জটিল ডিপেন্ডেন্সি সহ কোড টেস্ট করতে যাও, তাহলে তুমি মক অবজেক্ট তৈরি করার জন্য Mockito ব্যবহার করতে পারো। প্রথমে, তোমার `pubspec.yaml`-এ Mockito যোগ কর:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

ধর, তোমার কাছে একটি ক্লাস `UserRepository` আছে যেটি ইউজার ডাটা ফেচ করে, এবং তুমি একটি `UserService` টেস্ট করতে চাও যা `UserRepository`-এর উপর নির্ভর করে রিয়েল ডাটাবেইস হিট ছাড়াই:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// Mockito ব্যবহার করে একটি Mock ক্লাস তৈরি কর
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('UserService Tests', () {
    test('Fetches user successfully', () {
      // মক ইনস্ট্যান্স তৈরি কর
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // মক আচরণ সেট আপ কর
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Test User'));

      // মক মেথডটি প্রত্যাশিত আর্গুমেন্ট নিয়ে কল করা হয়েছে সেটা আশা করা
      expect(userService.getUserName(1), 'Test User');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

এই টেস্ট রান করা নিশ্চিত করে যে `UserService` ঠিকমতো `UserRepository`-এর সাথে ইন্টারেক্ট করেছে, মকিং ব্যবহার করে রিয়েল ইন্টারেকশনগুলিকে একটি নিয়ন্ত্রিত উপায়ে সিমুলেট করে।
