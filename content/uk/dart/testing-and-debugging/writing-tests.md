---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:05.316629-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Dart \u0437\u0430\u0437\u0432\u0438\u0447\u0430\u0439 \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F \u043F\
  \u0430\u043A\u0435\u0442 `test` \u0434\u043B\u044F \u043D\u0430\u043F\u0438\u0441\
  \u0430\u043D\u043D\u044F \u0442\u0435\u0441\u0442\u0456\u0432. \u0421\u043F\u043E\
  \u0447\u0430\u0442\u043A\u0443 \u0434\u043E\u0434\u0430\u0439\u0442\u0435 \u043F\
  \u0430\u043A\u0435\u0442 `test` \u0434\u043E \u0432\u0430\u0448\u043E\u0433\u043E\
  \ `pubspec.yaml`."
lastmod: '2024-03-13T22:44:48.804222-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Dart \u0437\u0430\u0437\u0432\u0438\u0447\u0430\u0439 \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F\
  \ \u043F\u0430\u043A\u0435\u0442 `test` \u0434\u043B\u044F \u043D\u0430\u043F\u0438\
  \u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\u0442\u0456\u0432."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\u0442\
  \u0456\u0432"
weight: 36
---

## Як це зробити:
У Dart зазвичай використовується пакет `test` для написання тестів. Спочатку додайте пакет `test` до вашого `pubspec.yaml`:

```yaml
dev_dependencies:
  test: ^1.0.0
```

Потім напишіть тест для простої функції. Припустімо, у вас є функція, яка додає два числа:

```dart
int add(int a, int b) {
  return a + b;
}
```

Далі створіть файл під назвою `add_test.dart` у директорії `test` та напишіть ваш тестовий випадок:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // Припускаємо, що ваша функція `add` знаходиться у lib/add.dart

void main() {
  test('додає два числа', () {
    var expected = 3;
    expect(add(1, 2), equals(expected));
  });
}
```

Щоб запустити тести, використовуйте команду Dart:

```bash
$ dart test
```

Зразок виводу може виглядати наступним чином:

```
00:01 +1: Усі тести пройдено!
```

### Використання сторонньої бібліотеки: Mockito для мокінгу
Для тестування коду, що має складні залежності, ви можете використовувати Mockito для створення мок-об'єктів. Спочатку додайте Mockito до вашого `pubspec.yaml`:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

Припустимо у вас є клас `UserRepository`, що отримує дані користувача, і ви хочете протестувати `UserService`, який залежить від `UserRepository` без звернення до справжньої бази даних:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// Створюємо мок-клас за допомогою Mockito
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('Тести UserService', () {
    test('Успішно отримує дані користувача', () {
      // Створення мок-інстансу
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // Встановлення поведінки моку
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Тестовий користувач'));

      // Переконуємося, що мокований метод викликаний з очікуваними аргументами
      expect(userService.getUserName(1), 'Тестовий користувач');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

Запуск цього тесту підтверджує, що `UserService` вірно інтерактує з `UserRepository`, використовуючи мокінг для симуляції реальних взаємодій контрольованим способом.
