---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:47.688673-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041E\u0441\u043D\u043E\u0432\u043D\u0430 \u0431\u0456\u0431\u043B\u0456\u043E\
  \u0442\u0435\u043A\u0430 Dart \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u043F\u0456\
  \u0434\u0442\u0440\u0438\u043C\u043A\u0443 \u0434\u043B\u044F \u0433\u0435\u043D\
  \u0435\u0440\u0430\u0446\u0456\u0457 \u0432\u0438\u043F\u0430\u0434\u043A\u043E\u0432\
  \u0438\u0445 \u0447\u0438\u0441\u0435\u043B \u0437\u0430 \u0434\u043E\u043F\u043E\
  \u043C\u043E\u0433\u043E\u044E \u043A\u043B\u0430\u0441\u0443 `Random`, \u0449\u043E\
  \ \u0437\u043D\u0430\u0445\u043E\u0434\u0438\u0442\u044C\u0441\u044F \u0432 `dart:math`.\
  \ \u041E\u0441\u044C\u2026"
lastmod: '2024-03-13T22:44:48.790067-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0441\u043D\u043E\u0432\u043D\u0430 \u0431\u0456\u0431\u043B\u0456\
  \u043E\u0442\u0435\u043A\u0430 Dart \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u043F\
  \u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0443 \u0434\u043B\u044F \u0433\u0435\
  \u043D\u0435\u0440\u0430\u0446\u0456\u0457 \u0432\u0438\u043F\u0430\u0434\u043A\u043E\
  \u0432\u0438\u0445 \u0447\u0438\u0441\u0435\u043B \u0437\u0430 \u0434\u043E\u043F\
  \u043E\u043C\u043E\u0433\u043E\u044E \u043A\u043B\u0430\u0441\u0443 `Random`, \u0449\
  \u043E \u0437\u043D\u0430\u0445\u043E\u0434\u0438\u0442\u044C\u0441\u044F \u0432\
  \ `dart:math`."
title: "\u0413\u0435\u043D\u0435\u0440\u0430\u0446\u0456\u044F \u0432\u0438\u043F\u0430\
  \u0434\u043A\u043E\u0432\u0438\u0445 \u0447\u0438\u0441\u0435\u043B"
weight: 12
---

## Як це зробити:
Основна бібліотека Dart включає підтримку для генерації випадкових чисел за допомогою класу `Random`, що знаходиться в `dart:math`. Ось базовий приклад:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // Генерує випадкове ціле число між 0 та 99
  double randomDouble = rand.nextDouble(); // Генерує випадкове дрібне число між 0.0 та 1.0
  print(randomNumber);
  print(randomDouble);
}
```

*Приклад виведення: (Буде відрізнятися при кожному запуску)*

```
23
0.6722390975465775
```

Для випадків, що потребують криптографічної непередбачуваності, Dart пропонує конструктор `Random.secure`:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*Приклад виведення: (Буде відрізнятися при кожному запуску)*

```
45
```

Якщо ви працюєте над проектами на Flutter або потребуєте більш складної випадковості, вам може бути корисним пакет `faker` для генерації широкого діапазону випадкових даних, таких як імена, адреси та дати.

Для використання `faker`, спочатку додайте його до вашого файлу `pubspec.yaml`:

```yaml
dependencies:
  faker: ^2.0.0
```

Потім імпортуйте його та використовуйте так, як показано:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Генерує випадкове ім'я
  print(faker.address.city()); // Генерує випадкову назву міста
}
```

*Приклад виведення:*

```
Josie Runolfsdottir
East Lysanne
```
