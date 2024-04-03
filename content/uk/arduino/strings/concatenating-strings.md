---
date: 2024-01-20 17:34:10.520027-07:00
description: "How to / \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438 ."
lastmod: '2024-03-13T22:44:49.706405-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041E\u0431'\u0454\u0434\u043D\u0430\u043D\u043D\u044F \u0440\u044F\u0434\
  \u043A\u0456\u0432"
weight: 3
---

## How to / Як це зробити
```Arduino
String partOne = "Привіт, ";
String partTwo = "світ!";
String combined = partOne + partTwo; // Конкатенація

Serial.begin(9600);
Serial.println(combined); // Вивід: Привіт, світ!
```

With character arrays:
```Arduino
char partOne[] = "Привіт, ";
char partTwo[] = "світ!";
char combined[20]; // Це повинно бути достатньо для обох рядків та нульового символу

strcat(combined, partOne);
strcat(combined, partTwo);

Serial.begin(9600);
Serial.println(combined); // Вивід: Привіт, світ!
```

## Deep Dive / Поглиблений Огляд
In early computing, memory was scarce, so efficient string handling was crucial. Arduino follows C/C++ string handling: straightforward concatenation with `+` for `String` objects, `strcat` for C-style character arrays.

Alternatives like `sprintf()` exist but are overkill for simple concatenation. Remember, `String` objects can cause memory fragmentation, so for larger projects, character arrays are safer.

Understanding the `null-terminating character` (`\0`) is key. Every character array meant to be a string ends with `\0`, which tells functions where the string ends.

## See Also / Додатково
- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Arduino `strcat()` Reference: https://www.arduino.cc/reference/en/language/functions/c-strings/strcat/
- Arduino Memory Management: https://www.arduino.cc/en/Tutorial/Memory
