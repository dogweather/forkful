---
title:                "Інтерполяція рядків"
date:                  2024-01-20T17:50:06.046178-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? | Що і Чому?

String interpolation is plugging variables into strings. Programmers do it to create messages dynamically, merging text with data.
Інтерполяція рядків - це вставлення змінних у рядки. Програмісти використовують це для динамічного створення повідомлень, поєднуючи текст з даними.

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