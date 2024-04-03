---
date: 2024-01-20 18:02:38.416096-07:00
description: "Starting a new Arduino project means initializing a fresh sketch to\
  \ build your unique idea. Programmers kick off new projects to solve problems, learn,\
  \ or\u2026"
lastmod: '2024-03-13T22:44:49.720778-06:00'
model: gpt-4-1106-preview
summary: Starting a new Arduino project means initializing a fresh sketch to build
  your unique idea.
title: "\u041F\u043E\u0447\u0438\u043D\u0430\u0454\u043C\u043E \u043D\u043E\u0432\u0438\
  \u0439 \u043F\u0440\u043E\u0435\u043A\u0442"
weight: 1
---

## How to: (Як це зробити:)
```Arduino
void setup() {
  // This runs once
  Serial.begin(9600);
  Serial.println("Hello, Ukraine!");
}

void loop() {
  // This runs repeatedly
}
```
Sample Output:
```
Hello, Ukraine!
```

## Deep Dive (Занурення у глибину)
Creating a new project in the Arduino environment means you're starting with two essential functions: `setup()` and `loop()`. Historically, sketches begin here, rooted in Processing and Wiring, ancestors of Arduino's programming language. Alternatives like Atmel Studio or PlatformIO offer more features but add complexity. The `setup()` function prepares your board setup once, while `loop()` lets you run code repeatedly. By keeping structure simple—it's quicker for debugging and learning.

## See Also (Дивіться також)
- [Arduino Language Reference](https://www.arduino.cc/reference/en/)
- [Getting Started with Arduino](https://www.arduino.cc/en/Guide)
- [Introduction to Arduino Programming](https://www.arduino.cc/en/Tutorial/BuiltInExamples)
