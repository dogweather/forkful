---
title:                "Обработка ошибок"
date:                  2024-03-08T21:55:05.691186-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Обработка ошибок в Dart связана с предвидением и управлением исключениями, возникающими во время выполнения программы, для повышения её надежности и удобства использования. Программисты реализуют обработку ошибок, чтобы предотвратить сбои и предоставить пользователям значимую обратную связь, обеспечивая более плавный и безопасный опыт приложения.

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