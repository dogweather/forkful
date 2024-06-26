---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:19.736994-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041F\u0435\u0440\u0435\u0434 \u0440\u0435\u0444\u0430\u043A\u0442\u043E\u0440\
  \u0438\u043D\u0433\u043E\u043C \u0443 \u0432\u0430\u0441 \u043C\u043E\u0436\u0435\
  \ \u0431\u0443\u0442\u0438 \u0448\u043C\u0430\u0442\u043E\u043A \u043A\u043E\u0434\
  \u0443, \u044F\u043A\u0438\u0439 \u043C\u0456\u0448\u0430\u0454 \u0440\u0456\u0437\
  \u043D\u0456 \u0440\u0456\u0432\u043D\u0456 \u0430\u0431\u0441\u0442\u0440\u0430\
  \u043A\u0446\u0456\u0457 \u0430\u0431\u043E \u043E\u0431\u043E\u0432'\u044F\u0437\
  \u043A\u0438, \u043D\u0430\u043F\u0440\u0438\u043A\u043B\u0430\u0434, \u043E\u0431\
  \u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0437\u043D\u0438\u0436\u043A\u0438\
  , \u0430 \u043F\u043E\u0442\u0456\u043C \u0457\u0457\u2026"
lastmod: '2024-04-05T21:53:49.045176-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0435\u0440\u0435\u0434 \u0440\u0435\u0444\u0430\u043A\u0442\u043E\
  \u0440\u0438\u043D\u0433\u043E\u043C \u0443 \u0432\u0430\u0441 \u043C\u043E\u0436\
  \u0435 \u0431\u0443\u0442\u0438 \u0448\u043C\u0430\u0442\u043E\u043A \u043A\u043E\
  \u0434\u0443, \u044F\u043A\u0438\u0439 \u043C\u0456\u0448\u0430\u0454 \u0440\u0456\
  \u0437\u043D\u0456 \u0440\u0456\u0432\u043D\u0456 \u0430\u0431\u0441\u0442\u0440\
  \u0430\u043A\u0446\u0456\u0457 \u0430\u0431\u043E \u043E\u0431\u043E\u0432'\u044F\
  \u0437\u043A\u0438, \u043D\u0430\u043F\u0440\u0438\u043A\u043B\u0430\u0434, \u043E\
  \u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0437\u043D\u0438\u0436\u043A\
  \u0438, \u0430 \u043F\u043E\u0442\u0456\u043C \u0457\u0457 \u0437\u0430\u0441\u0442\
  \u043E\u0441\u0443\u0432\u0430\u043D\u043D\u044F."
title: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433"
weight: 19
---

## Як це зробити:


### Приклад 1: Перейменування та виділення методів
Перед рефакторингом у вас може бути шматок коду, який мішає різні рівні абстракції або обов'язки, наприклад, обчислення знижки, а потім її застосування:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("Кінцева ціна: $finalPrice");
}
```
**Вивід:**
```
Кінцева ціна: 80.0
```

Після рефакторингу ви можете виділити обчислення знижки в окремий метод і дати йому значуще ім'я:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = calculateFinalPrice(price, discount);
  print("Кінцева ціна: $finalPrice");
}

double calculateFinalPrice(double price, double discount) {
  return price - (price * discount);
}
```
**Вивід:**
```
Кінцева ціна: 80.0
```

Виділивши обчислення в метод, у вас тепер є чітко визначена операція, яка може бути повторно використана, протестована незалежно та легко модифікована.

### Приклад 2: Спрощення умовних виразів
Перед рефакторингом, умовні оператори можуть бути надмірно складними або важкими для читання:

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

  print("Знижка: $discount");
}
```
**Вивід:**
```
Знижка: 0.05
```

Після рефакторингу, розгляньте можливість використання мапи для чіткішої структури та легших оновлень або розширень типів клієнтів і знижок:

```dart
void main() {
  var customerType = "regular";
  var discounts = {
    "regular": 0.05,
    "member": 0.1,
    "none": 0.0,
  };

  var discount = discounts[customerType] ?? 0.0;
  print("Знижка: $discount");
}
```
**Вивід:**
```
Знижка: 0.05
```

Цей рефакторинг не тільки робить код більш лаконічним, але й укладає логіку визначення знижок у спосіб, який легше розуміти та підтримувати.

### Бібліотеки від сторонніх розробників для рефакторингу
Коли мова йде про рефакторинг у Dart, особливо в межах додатків Flutter, незамінним є набір інструментів [Dart DevTools](https://dart.dev/tools/dart-devtools). Він включає інструменти для аналізу продуктивності, інспектор віджетів та відладчик на рівні коду. Хоча Dart DevTools не є бібліотекою від сторонніх розробників, його часто використовують разом з бібліотеками, такими як `flutter_bloc`, для чистого керування станом у спосіб, що сприяє рефакторингу з метою покращення модульності та читабельності. На жаль, через обмеження цього вступу, конкретні приклади коду, що використовують бібліотеки від сторонніх розробників, тут не надаються, але розробникам рекомендується досліджувати ці інструменти для покращення процесу рефакторингу у своїх додатках Dart/Flutter.
