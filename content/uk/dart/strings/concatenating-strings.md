---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:53.783113-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Dart \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u043A\u0456\u043B\u044C\
  \u043A\u0430 \u043F\u0440\u043E\u0441\u0442\u0438\u0445 \u0441\u043F\u043E\u0441\
  \u043E\u0431\u0456\u0432 \u043A\u043E\u043D\u043A\u0430\u0442\u0435\u043D\u0430\u0446\
  \u0456\u0457 \u0440\u044F\u0434\u043A\u0456\u0432. \u041D\u0438\u0436\u0447\u0435\
  \ \u043D\u0430\u0432\u0435\u0434\u0435\u043D\u043E \u043D\u0430\u0439\u043F\u043E\
  \u0448\u0438\u0440\u0435\u043D\u0456\u0448\u0456 \u043C\u0435\u0442\u043E\u0434\u0438\
  ."
lastmod: '2024-04-05T21:53:49.016766-06:00'
model: gpt-4-0125-preview
summary: "Dart \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u043A\u0456\u043B\
  \u044C\u043A\u0430 \u043F\u0440\u043E\u0441\u0442\u0438\u0445 \u0441\u043F\u043E\
  \u0441\u043E\u0431\u0456\u0432 \u043A\u043E\u043D\u043A\u0430\u0442\u0435\u043D\u0430\
  \u0446\u0456\u0457 \u0440\u044F\u0434\u043A\u0456\u0432."
title: "\u041A\u043E\u043D\u043A\u0430\u0442\u0435\u043D\u0430\u0446\u0456\u044F \u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 3
---

## Як це зробити:
Dart пропонує кілька простих способів конкатенації рядків. Нижче наведено найпоширеніші методи:

### Використання оператора `+`
Оператор `+` є найінтуїтивнішим способом приєднати рядки.
```dart
String greeting = 'Привіт, ' + 'Світ!';
print(greeting); // Вивід: Привіт, Світ!
```

### Використання методу `concat()`
Хоча Dart не має методу `concat()`, подібного до інших мов, цього можна досягти, використовуючи `+` або наступні методи.

### Використання інтерполяції рядків
Інтерполяція рядків дозволяє безпосередньо вбудовувати змінні в рядок. Це ефективно для поєднання рядків та виразів.
```dart
String user = 'Джейн';
String message = 'Ласкаво просимо, $user!';
print(message); // Вивід: Ласкаво просимо, Джейн!
```

### Використання методу `join()`
Метод `join()` корисний, коли у вас є список рядків, які ви хочете об'єднати.
```dart
var words = ['Привіт', 'з', 'Dart'];
String sentence = words.join(' '); // Об'єднати з роздільником-пробілом.
print(sentence); // Вивід: Привіт з Dart
```

### Використання StringBuffer
`StringBuffer` ефективний для кількох конкатенацій, особливо в циклах.
```dart
var words = ['Dart', 'це', 'весело'];
StringBuffer buffer = StringBuffer();
for (String word in words) {
  buffer.write(word); // Додати кожне слово до буфера.
  buffer.write(' '); // За бажанням додати пробіл.
}
String sentence = buffer.toString().trim(); // Конвертувати в рядок та видалити кінцевий пробіл.
print(sentence); // Вивід: Dart це весело
```

### Сторонні бібліотеки
Хоча стандартна бібліотека Dart зазвичай достатня для завдань конкатенації рядків, сторонні бібліотеки, як-от `quiver`, пропонують утиліти, які можуть доповнити вбудовані можливості Dart. Наприклад, можна дослідити функції `concat()` або `merge()` від `quiver` для складніших сценаріїв. Однак, користуйтеся міцними вбудованими опціями Dart, якщо у вас немає конкретної потреби, яку вони не покривають.
