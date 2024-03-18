---
title:                "টেস্ট লিখা"
date:                  2024-03-17T18:40:56.367420-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

Dart-এ টেস্ট লেখার মানে হল ডিফারেন্ট পার্টস অফ ইওর প্রোগ্রাম ঠিকমতো কাজ করছে কিনা সেটা যাচাই করার জন্য টেস্ট কেস অটোমেটিক্যালি তৈরি করা। প্রোগ্রামাররা তাদের কোড নির্ভরযোগ্য এবং ত্রুটিমুক্ত রাখার জন্য এটি করে, যা আরো সহজে আপডেট এবং রিফ্যাক্টরিং করার সহায়তা করে এবং রিগ্রেশন প্রতিরোধ করে।

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
