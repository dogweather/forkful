---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:06.544131-07:00
description: "\u0406\u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\u0432\u043D\u0438\
  \u0439 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u0438\u0439 \u0456\u043D\u0442\
  \u0435\u0440\u043F\u0440\u0435\u0442\u0430\u0442\u043E\u0440 (REPL - Read-Evaluate-Print\
  \ Loop) \u0434\u043B\u044F Dart \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0430\u043C \u0434\
  \u0438\u043D\u0430\u043C\u0456\u0447\u043D\u043E \u0432\u0432\u043E\u0434\u0438\u0442\
  \u0438 \u0456 \u0432\u0438\u043A\u043E\u043D\u0443\u0432\u0430\u0442\u0438 \u043A\
  \u043E\u0434 Dart \u0440\u044F\u0434\u043E\u043A \u0437\u0430\u2026"
lastmod: '2024-03-13T22:44:48.800528-06:00'
model: gpt-4-0125-preview
summary: "\u0406\u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\u0432\u043D\u0438\
  \u0439 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u0438\u0439 \u0456\u043D\u0442\
  \u0435\u0440\u043F\u0440\u0435\u0442\u0430\u0442\u043E\u0440 (REPL - Read-Evaluate-Print\
  \ Loop) \u0434\u043B\u044F Dart \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0430\u043C \u0434\
  \u0438\u043D\u0430\u043C\u0456\u0447\u043D\u043E \u0432\u0432\u043E\u0434\u0438\u0442\
  \u0438 \u0456 \u0432\u0438\u043A\u043E\u043D\u0443\u0432\u0430\u0442\u0438 \u043A\
  \u043E\u0434 Dart \u0440\u044F\u0434\u043E\u043A \u0437\u0430\u2026"
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0456\
  \u043D\u0442\u0435\u0440\u0430\u043A\u0442\u0438\u0432\u043D\u043E\u0457 \u043E\u0431\
  \u043E\u043B\u043E\u043D\u043A\u0438 (REPL)"
---

{{< edit_this_page >}}

## Що та Чому?

Інтерактивний командний інтерпретатор (REPL - Read-Evaluate-Print Loop) для Dart дозволяє програмістам динамічно вводити і виконувати код Dart рядок за рядком, не потребуючи компіляції всього скрипту. Цей інструмент незамінний для вивчення синтаксису Dart, експериментування з фрагментами коду або відлагодження, оскільки надає миттєву відповідь та сприяє ітеративному тестуванню.

## Як користуватися:

Dart не має вбудованого REPL. Однак, REPL-подібний функціонал можна досягти, використовуючи DartPad (онлайн) або за допомогою сторонніх інструментів, таких як `dart_repl`.

**Використання DartPad:**

DartPad (https://dartpad.dev) - це онлайн-редактор Dart, який дозволяє писати та виконувати код Dart у вашому веб-браузері. Хоча це не традиційний командно-рядковий REPL, він забезпечує схожий досвід для швидких експериментів.

Просто перейдіть на вебсайт, введіть свій код Dart у лівій панелі та натисніть "Виконати", щоб побачити результат у правій.

Приклад:
```dart
void main() {
  print('Привіт, Dart!');
}
```
Вивід:
```
Привіт, Dart!
```

**Використання `dart_repl` (сторонній інструмент):**

Спочатку встановіть `dart_repl` через pub глобально:

```shell
dart pub global activate dart_repl
```

Потім запустіть `dart_repl` з вашого терміналу:

```shell
dart_repl
```

Тепер ви можете почати вводити вирази Dart прямо в оболонку. Наприклад:

```dart
>>> print('Привіт, REPL!');
Привіт, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

Ці методи надають швидкий шлях для експериментування з кодом Dart "на льоту", значно полегшуючи процес навчання та підвищуючи продуктивність.
