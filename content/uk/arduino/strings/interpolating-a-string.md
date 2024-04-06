---
date: 2024-01-20 17:50:06.046178-07:00
description: "How to: | \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438: Output."
lastmod: '2024-04-05T21:53:49.854342-06:00'
model: gpt-4-1106-preview
summary: ''
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
