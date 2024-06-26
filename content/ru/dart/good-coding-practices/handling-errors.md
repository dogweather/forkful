---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:05.691186-07:00
description: "\u041A\u0430\u043A: Dart \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u0438\
  \u0432\u0430\u0435\u0442 \u0434\u0432\u0430 \u0442\u0438\u043F\u0430 \u043E\u0448\
  \u0438\u0431\u043E\u043A: \u043E\u0448\u0438\u0431\u043A\u0438 *\u0432\u0440\u0435\
  \u043C\u0435\u043D\u0438 \u043A\u043E\u043C\u043F\u0438\u043B\u044F\u0446\u0438\u0438\
  * \u0438 \u043E\u0448\u0438\u0431\u043A\u0438 *\u0432\u0440\u0435\u043C\u0435\u043D\
  \u0438 \u0432\u044B\u043F\u043E\u043B\u043D\u0435\u043D\u0438\u044F*. \u041E\u0448\
  \u0438\u0431\u043A\u0438 \u0432\u0440\u0435\u043C\u0435\u043D\u0438 \u043A\u043E\
  \u043C\u043F\u0438\u043B\u044F\u0446\u0438\u0438 \u043E\u0431\u043D\u0430\u0440\u0443\
  \u0436\u0438\u0432\u0430\u044E\u0442\u0441\u044F \u0430\u043D\u0430\u043B\u0438\u0437\
  \u0430\u0442\u043E\u0440\u043E\u043C\u2026"
lastmod: '2024-03-13T22:44:44.525916-06:00'
model: gpt-4-0125-preview
summary: "Dart \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u0438\u0432\u0430\u0435\u0442\
  \ \u0434\u0432\u0430 \u0442\u0438\u043F\u0430 \u043E\u0448\u0438\u0431\u043E\u043A\
  ."
title: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\u0431\
  \u043E\u043A"
weight: 16
---

## Как:
Dart поддерживает два типа ошибок: ошибки *времени компиляции* и ошибки *времени выполнения*. Ошибки времени компиляции обнаруживаются анализатором Dart до выполнения кода, в то время как ошибки времени выполнения, или исключения, возникают во время выполнения. Вот как обрабатывать исключения в Dart:

### Try-Catch
Используйте `try-catch` для перехвата исключений и предотвращения их сбоев в вашем приложении:

```dart
try {
  var result = 100 ~/ 0; // Попытка деления на ноль, вызывает исключение
} catch (e) {
  print('Перехвачено исключение: $e'); // Обрабатывает исключение
}
```
Пример вывода: `Перехвачено исключение: IntegerDivisionByZeroException`

### Конкретное Исключение
Для обработки конкретных исключений укажите исключение после `catch`:

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('Нельзя делить на ноль.'); // Специально обрабатывает исключения при делении на ноль
}
```
Пример вывода: `Нельзя делить на ноль.`

### Стек Вызовов
Чтобы получить стек вызовов для отладки, используйте второй параметр в блоке catch:

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('Исключение: $e');
  print('Стек вызовов: $s'); // Выводит стек вызовов для отладки
}
```

### Finally
Используйте `finally` для выполнения кода после try/catch, независимо от того, было ли выброшено исключение:

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('Перехвачено исключение: $e');
} finally {
  print('Это выполняется всегда.'); // Код очистки или завершающие шаги
}
```
Пример вывода:
```
Перехвачено исключение: IntegerDivisionByZeroException
Это выполняется всегда.
```

### Сторонние Библиотеки
Хотя основная библиотека Dart надежна для обработки ошибок, вы также можете использовать сторонние пакеты, такие как `dartz` для функционального программирования, которые вводят концепции, такие как `Either` и `Option`, которые могут быть использованы для обработки ошибок. Вот пример использования `dartz` для обработки ошибок:

1. Добавьте `dartz` в файл `pubspec.yaml` в раздел зависимостей:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. Используйте `Either` для изящной обработки ошибок в вашем коде Dart:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividend, int divisor) {
  if (divisor == 0) {
    return Left('Нельзя делить на ноль.');
  } else {
    return Right(dividend ~/ divisor);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('Ошибка: $left'), 
    (right) => print('Результат: $right')
  );
}
```
Пример вывода: `Ошибка: Нельзя делить на ноль.`

Часть `Left` обычно представляет ошибку, а часть `Right` - успех. Этот шаблон позволяет обрабатывать ошибки более функциональным способом, обеспечивая ясность и контроль над управлением ошибками.
