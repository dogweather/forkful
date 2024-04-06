---
date: 2024-01-20 17:50:06.046178-07:00
description: "How to: | \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438: Historically, C programmers used sprintf - Arduino inherited this. `+` is\
  \ easier for simple strings, but sprintf offers\u2026"
lastmod: '2024-04-05T22:51:02.704611-06:00'
model: gpt-4-1106-preview
summary: "| \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:\
  \ Historically, C programmers used sprintf - Arduino inherited this. `+` is easier\
  \ for simple strings, but sprintf offers formatting. Arduino doesn't support some\
  \ advanced interpolation features like `printf` found in languages like Python.\
  \ Interpolating strings efficiently is important to manage memory on low-power devices.\
  \ \u0406\u0441\u0442\u043E\u0440\u0438\u0447\u043D\u043E \u043F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u043D\u0430 \u043C\u043E\u0432\u0456\
  \ C \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\
  \u043B\u0438 sprintf \u2014 Arduino \u0443\u0441\u043F\u0430\u0434\u043A\u0443\u0432\
  \u0430\u043B\u043E \u0446\u0435. `+` \u043F\u0440\u043E\u0441\u0442\u0456\u0448\u0435\
  \ \u0434\u043B\u044F \u043B\u0435\u0433\u043A\u0438\u0445 \u0440\u044F\u0434\u043A\
  \u0456\u0432, \u0430\u043B\u0435 sprintf \u043F\u0440\u043E\u043F\u043E\u043D\u0443\
  \u0454 \u0444\u043E\u0440\u043C\u0430\u0442\u0443\u0432\u0430\u043D\u043D\u044F\
  . Arduino \u043D\u0435 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u0443\u0454 \u0434\
  \u0435\u044F\u043A\u0456 \u0440\u043E\u0437\u0448\u0438\u0440\u0435\u043D\u0456\
  \ \u043C\u043E\u0436\u043B\u0438\u0432\u043E\u0441\u0442\u0456 \u0456\u043D\u0442\
  \u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u0457 \u0440\u044F\u0434\u043A\u0456\
  \u0432, \u0442\u0430\u043A\u0456 \u044F\u043A `printf`, \u0449\u043E \u0434\u043E\
  \u0441\u0442\u0443\u043F\u043D\u0456 \u0432 \u043C\u043E\u0432\u0430\u0445 \u043F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043D\u043D\u044F \u0442\u0438\
  \u043F\u0443 Python. \u0415\u0444\u0435\u043A\u0442\u0438\u0432\u043D\u0430 \u0456\
  \u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u044F \u0440\u044F\u0434\
  \u043A\u0456\u0432 \u0432\u0430\u0436\u043B\u0438\u0432\u0430 \u0434\u043B\u044F\
  \ \u0443\u043F\u0440\u0430\u0432\u043B\u0456\u043D\u043D\u044F \u043F\u0430\u043C\
  \u2019\u044F\u0442\u0442\u044E \u043D\u0430 \u043F\u0440\u0438\u0441\u0442\u0440\
  \u043E\u044F\u0445 \u0437 \u043E\u0431\u043C\u0435\u0436\u0435\u043D\u0438\u043C\
  \u0438 \u0440\u0435\u0441\u0443\u0440\u0441\u0430\u043C\u0438."
title: "\u0406\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u044F \u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 8
---

## How to: | Як це зробити:
```Arduino
String name = "Andriy";
int age = 25;

// Concatenation
String greeting = "Hello, " + name + "! You are " + age + " years old.";
Serial.println(greeting);

// With sprintf (C style)
char buf[50];
sprintf(buf, "Hello, %s! You are %d years old.", name.c_str(), age);
Serial.println(buf);
```
Output:
```
Hello, Andriy! You are 25 years old.
Hello, Andriy! You are 25 years old.
```

## Deep Dive | Поглиблений Аналіз:
Historically, C programmers used sprintf - Arduino inherited this. `+` is easier for simple strings, but sprintf offers formatting. Arduino doesn't support some advanced interpolation features like `printf` found in languages like Python. Interpolating strings efficiently is important to manage memory on low-power devices.

Історично програмісти на мові C використовували sprintf — Arduino успадкувало це. `+` простіше для легких рядків, але sprintf пропонує форматування. Arduino не підтримує деякі розширені можливості інтерполяції рядків, такі як `printf`, що доступні в мовах програмування типу Python. Ефективна інтерполяція рядків важлива для управління пам’яттю на пристроях з обмеженими ресурсами.

## See Also | Дивіться Також:
- Arduino `String` class reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- `sprintf()` documentation: https://www.cplusplus.com/reference/cstdio/sprintf/
- Arduino memory management: https://www.arduino.cc/en/Tutorial/Foundations/Memory
