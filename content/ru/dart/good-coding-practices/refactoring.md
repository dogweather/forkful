---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:23.137210-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0434\u0435\u043B\u0430\u0442\
  \u044C: \u0414\u043E \u0440\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\
  \u0433\u0430 \u0443 \u0432\u0430\u0441 \u043C\u043E\u0436\u0435\u0442 \u0431\u044B\
  \u0442\u044C \u043A\u0443\u0441\u043E\u043A \u043A\u043E\u0434\u0430, \u043A\u043E\
  \u0442\u043E\u0440\u044B\u0439 \u0441\u043C\u0435\u0448\u0438\u0432\u0430\u0435\u0442\
  \ \u0440\u0430\u0437\u043D\u044B\u0435 \u0443\u0440\u043E\u0432\u043D\u0438 \u0430\
  \u0431\u0441\u0442\u0440\u0430\u043A\u0446\u0438\u0438 \u0438\u043B\u0438 \u043E\
  \u0442\u0432\u0435\u0442\u0441\u0442\u0432\u0435\u043D\u043D\u043E\u0441\u0442\u0438\
  , \u043D\u0430\u043F\u0440\u0438\u043C\u0435\u0440, \u0440\u0430\u0441\u0447\u0435\
  \u0442 \u0441\u043A\u0438\u0434\u043A\u0438, \u0430\u2026"
lastmod: '2024-04-05T21:53:45.110548-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043E \u0440\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\
  \u0433\u0430 \u0443 \u0432\u0430\u0441 \u043C\u043E\u0436\u0435\u0442 \u0431\u044B\
  \u0442\u044C \u043A\u0443\u0441\u043E\u043A \u043A\u043E\u0434\u0430, \u043A\u043E\
  \u0442\u043E\u0440\u044B\u0439 \u0441\u043C\u0435\u0448\u0438\u0432\u0430\u0435\u0442\
  \ \u0440\u0430\u0437\u043D\u044B\u0435 \u0443\u0440\u043E\u0432\u043D\u0438 \u0430\
  \u0431\u0441\u0442\u0440\u0430\u043A\u0446\u0438\u0438 \u0438\u043B\u0438 \u043E\
  \u0442\u0432\u0435\u0442\u0441\u0442\u0432\u0435\u043D\u043D\u043E\u0441\u0442\u0438\
  , \u043D\u0430\u043F\u0440\u0438\u043C\u0435\u0440, \u0440\u0430\u0441\u0447\u0435\
  \u0442 \u0441\u043A\u0438\u0434\u043A\u0438, \u0430 \u0437\u0430\u0442\u0435\u043C\
  \ \u0435\u0435 \u043F\u0440\u0438\u043C\u0435\u043D\u0435\u043D\u0438\u0435."
title: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433"
weight: 19
---

## Как это делать:


### Пример 1: Переименование и извлечение методов
До рефакторинга у вас может быть кусок кода, который смешивает разные уровни абстракции или ответственности, например, расчет скидки, а затем ее применение:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("Финальная цена: $finalPrice");
}
```

**Вывод:**
```
Финальная цена: 80.0
```

После рефакторинга вы можете извлечь расчет скидки в свой собственный метод и дать ему значимое имя:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = calculateFinalPrice(price, discount);
  print("Финальная цена: $finalPrice");
}

double calculateFinalPrice(double price, double discount) {
  return price - (price * discount);
}
```

**Вывод:**
```
Финальная цена: 80.0
```

Извлекая расчет в метод, вы теперь имеете четко определенную операцию, которая может быть повторно использована, независимо протестирована и легко изменена.

### Пример 2: Упрощение условных выражений
До рефакторинга условные операторы могут быть слишком сложными или трудночитаемыми:

```dart
void main() {
  var customerType = "regular";
  double discount;
  
  if (customerType == "regular") {
    discount = 0.05;
  } else if (customerType == "member") {
    discount = 0.1;
  } else {
    discount = 0.0;
  }

  print("Скидка: $discount");
}
```

**Вывод:**
```
Скидка: 0.05
```

После рефакторинга рассмотрите использование карты для более четкой структуры и упрощения обновлений или расширений типов клиентов и скидок:

```dart
void main() {
  var customerType = "regular";
  var discounts = {
    "regular": 0.05,
    "member": 0.1,
    "none": 0.0,
  };

  var discount = discounts[customerType] ?? 0.0;
  print("Скидка: $discount");
}
```

**Вывод:**
```
Скидка: 0.05
```

Этот рефакторинг не только делает код более сжатым, но также инкапсулирует логику определения скидок таким образом, что понимать и поддерживать ее становится легче.

### Сторонние библиотеки для рефакторинга
Когда речь идет о рефакторинге в Dart, особенно в приложениях Flutter, набор инструментов [Dart DevTools](https://dart.dev/tools/dart-devtools) оказывается бесценным. В него входят инструменты для анализа производительности, инспектор виджетов и отладчик на уровне исходного кода. Хотя это и не сторонняя библиотека, Dart DevTools часто используется вместе с библиотеками вроде `flutter_bloc` для чистого управления состоянием, что способствует рефакторингу с целью повышения модульности и читаемости. К сожалению, из-за ограниченности данной статьи, конкретные примеры кода, использующего сторонние библиотеки, здесь не будут приведены, но разработчикам рекомендуется изучить эти инструменты для улучшения процесса рефакторинга в своих приложениях на Dart/Flutter.
