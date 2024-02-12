---
title:                "Починаємо новий проект"
aliases:
- /uk/arduino/starting-a-new-project/
date:                  2024-01-20T18:02:38.416096-07:00
model:                 gpt-4-1106-preview
simple_title:         "Починаємо новий проект"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Starting a new Arduino project means initializing a fresh sketch to build your unique idea. Programmers kick off new projects to solve problems, learn, or just for fun.

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
