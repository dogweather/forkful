---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:59.569816-07:00
description: "\u041A\u0430\u043A: \u0412 Dart \u0434\u043B\u044F \u043D\u0430\u043F\
  \u0438\u0441\u0430\u043D\u0438\u044F \u0442\u0435\u0441\u0442\u043E\u0432 \u043E\
  \u0431\u044B\u0447\u043D\u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\
  \u0442\u0441\u044F \u043F\u0430\u043A\u0435\u0442 `test`. \u0421\u043D\u0430\u0447\
  \u0430\u043B\u0430 \u0434\u043E\u0431\u0430\u0432\u044C\u0442\u0435 \u043F\u0430\
  \u043A\u0435\u0442 `test` \u0432 \u0444\u0430\u0439\u043B `pubspec.yaml`."
lastmod: '2024-03-13T22:44:44.518404-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Dart \u0434\u043B\u044F \u043D\u0430\u043F\u0438\u0441\u0430\u043D\
  \u0438\u044F \u0442\u0435\u0441\u0442\u043E\u0432 \u043E\u0431\u044B\u0447\u043D\
  \u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\u0441\u044F\
  \ \u043F\u0430\u043A\u0435\u0442 `test`."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
weight: 36
---

## Как:
В Dart для написания тестов обычно используется пакет `test`. Сначала добавьте пакет `test` в файл `pubspec.yaml`:

```yaml
dev_dependencies:
  test: ^1.0.0
```

Затем напишите тест для простой функции. Предположим, у вас есть функция, которая складывает два числа:

```dart
int add(int a, int b) {
  return a + b;
}
```

Далее, создайте файл с именем `add_test.dart` в директории `test` и напишите ваш тестовый случай:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // Предположим, что ваша функция `add` находится в lib/add.dart

void main() {
  test('складывает два числа', () {
    var expected = 3;
    expect(add(1, 2), equals(expected));
  });
}
```

Чтобы запустить тесты, используйте команду Dart:

```bash
$ dart test
```

Пример вывода может выглядеть так:

```
00:01 +1: Все тесты пройдены!
```

### Использование сторонней библиотеки: Mockito для создания моков
Для тестирования кода, имеющего сложные зависимости, вы можете использовать Mockito для создания моковых объектов. Сначала добавьте Mockito в ваш `pubspec.yaml`:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

Предположим, у вас есть класс `UserRepository`, который получает данные пользователя, и вы хотите протестировать `UserService`, зависящий от `UserRepository`, без обращения к реальной базе данных:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// Создаем класс Mock с использованием Mockito
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('Тесты UserService', () {
    test('Успешно извлекает данные пользователя', () {
      // Создаем моковый экземпляр
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // Настраиваем поведение мока
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Тестовый Пользователь'));

      // Утверждаем, что моковый метод вызывается с ожидаемыми аргументами
      expect(userService.getUserName(1), 'Тестовый Пользователь');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

Запуск этого теста подтверждает, что `UserService` корректно взаимодействует с `UserRepository`, используя моки для симуляции реальных взаимодействий контролируемым способом.
