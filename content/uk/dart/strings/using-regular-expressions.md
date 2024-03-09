---
title:                "Використання регулярних виразів"
date:                  2024-03-08T21:57:06.821806-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Регулярні вирази (регекси) в Dart пропонують потужний спосіб пошуку та маніпуляції рядками, дозволяючи програмістам ефективно виконувати складні задачі обробки тексту. Розуміння регексів дозволяє розробникам швидко виконувати валідацію тексту, пошук за шаблонами та перетворення тексту, що є важливим для обробки форм, парсингу даних і загальної маніпуляції рядками в сучасних додатках.

## Як це робити:
Dart використовує клас `RegExp` для регулярних виразів. Ось базовий приклад пошуку за простим шаблоном у рядку:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Вивчення програмування на Dart захоплює.';

  if (pattern.hasMatch(text)) {
    print('Збіг знайдено!');
  } else {
    print('Збігів не знайдено.');
  }
  // Вивід: Збіг знайдено!
}
```

Щоб витягти збіги з рядка, можна скористатися методом `allMatches`. Цей метод повертає ітерабельну колекцію збігів:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart круто!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // Це виводить відповідні підрядки.
  }
  // Вивід:
  // Dart
  // круто
}
```

Замінити текст можна, використовуючи методи `replaceFirst` або `replaceAll`:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart не просто дротик.';

  // Заміна першого збігу
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // Вивід: Flutter не просто дротик.

  // Заміна всіх збігів
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // Вивід: Flutter не просто flutter.
}
```

Розділення рядка за допомогою регекс-шаблону просте з методом `split`:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // Відповідає будь-якому символу пробілу
  var text = 'Dart весело';

  var parts = text.split(pattern);
  print(parts); 
  // Вивід: [Dart, весело]
}
```

Для складного парсингу чи валідацій, які не підтримуються безпосередньо регексами в Dart, можна розглянути використання сторонніх бібліотек, але стандартна бібліотека Dart зазвичай є достатньою для поширених регекс-задач, підкреслюючи її корисність і універсальність у роботі з регулярними виразами.