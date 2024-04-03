---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:43.355292-07:00
description: "\u041A\u0430\u043A: \u041E\u0441\u043D\u043E\u0432\u043D\u0430\u044F\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430 Dart \u0432\u043A\
  \u043B\u044E\u0447\u0430\u0435\u0442 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\
  \u0443 \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u0438 \u0441\u043B\u0443\
  \u0447\u0430\u0439\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\u043B \u0441 \u043F\
  \u043E\u043C\u043E\u0449\u044C\u044E \u043A\u043B\u0430\u0441\u0441\u0430 `Random`,\
  \ \u043D\u0430\u0439\u0434\u0435\u043D\u043D\u043E\u0433\u043E \u0432 `dart:math`.\
  \ \u0412\u043E\u0442 \u0431\u0430\u0437\u043E\u0432\u044B\u0439 \u043F\u0440\u0438\
  \u043C\u0435\u0440."
lastmod: '2024-03-13T22:44:44.503791-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0441\u043D\u043E\u0432\u043D\u0430\u044F \u0431\u0438\u0431\u043B\
  \u0438\u043E\u0442\u0435\u043A\u0430 Dart \u0432\u043A\u043B\u044E\u0447\u0430\u0435\
  \u0442 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0443 \u0433\u0435\u043D\
  \u0435\u0440\u0430\u0446\u0438\u0438 \u0441\u043B\u0443\u0447\u0430\u0439\u043D\u044B\
  \u0445 \u0447\u0438\u0441\u0435\u043B \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\
  \u044E \u043A\u043B\u0430\u0441\u0441\u0430 `Random`, \u043D\u0430\u0439\u0434\u0435\
  \u043D\u043D\u043E\u0433\u043E \u0432 `dart:math`."
title: "\u0413\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044F \u0441\u043B\u0443\u0447\
  \u0430\u0439\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\u043B"
weight: 12
---

## Как:
Основная библиотека Dart включает поддержку генерации случайных чисел с помощью класса `Random`, найденного в `dart:math`. Вот базовый пример:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // Генерирует случайное целое число между 0 и 99
  double randomDouble = rand.nextDouble(); // Генерирует случайное число с плавающей точкой между 0.0 и 1.0
  print(randomNumber);
  print(randomDouble);
}
```

*Пример вывода: (Это будет варьироваться каждый раз при запуске)*

```
23
0.6722390975465775
```

Для случаев использования, требующих криптографической случайности, Dart предлагает конструктор `Random.secure`:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*Пример вывода: (Это будет варьироваться каждый раз при запуске)*

```
45
```

Если вы работаете над проектами Flutter или вам нужна более сложная случайность, пакет `faker` может оказаться полезным для генерации широкого спектра случайных данных, таких как имена, адреса и даты.

Чтобы использовать `faker`, сначала добавьте его в ваш файл `pubspec.yaml`:

```yaml
dependencies:
  faker: ^2.0.0
```

Затем импортируйте и используйте его, как показано:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Генерирует случайное имя
  print(faker.address.city()); // Генерирует случайное название города
}
```

*Пример вывода:*

```
Josie Runolfsdottir
East Lysanne
```
