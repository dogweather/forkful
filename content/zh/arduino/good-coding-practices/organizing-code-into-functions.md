---
date: 2024-01-26 01:09:10.009895-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u60F3\u8C61\u4E00\u4E0B\uFF0C\u60A8\u60F3\
  \u8981\u8BA9\u4E00\u4E2A LED \u706F\u95EA\u70C1\u3002\u4E0D\u4F7F\u7528\u51FD\u6570\
  \uFF0C\u60A8\u7684 `loop` \u4F1A\u662F\u4E00\u56E2\u4E71\u9EBB\u3002\u4F7F\u7528\
  \u51FD\u6570\uFF0C\u5B83\u5C31\u4F1A\u6574\u6D01\u4E0D\u5C11\u3002\u4E0B\u9762\u662F\
  \u64CD\u4F5C\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T21:53:48.363899-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

## 如何操作：
想象一下，您想要让一个 LED 灯闪烁。不使用函数，您的 `loop` 会是一团乱麻。使用函数，它就会整洁不少。下面是操作方法：

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // 每500毫秒让LED灯闪烁一次
}

// 一个让LED灯闪烁的函数
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

示例输出：您的 LED 灯在愉快地闪烁，代码的目的一目了然。

## 深入探索
在有函数之前，编程就像一次线性的公路旅行；从开始到结束，你看到每一个坑洼。有了函数之后，它更像是跳跃航班 - 你可以跳到重要的部分。历史上，子程序（早期的函数）在编程中是一场革命，让编码人员避免了重复自己—这就是 DRY 原则，不要重复自己。函数的替代品可能包括宏命令或用于面向对象编程（OOP）的类。细节方面呢？当你定义一个函数时，你就是在给编译器一个执行任务的蓝图。使用 Arduino，你通常定义的是 void 函数，它们充当微控制器的简单指令，但函数也可以返回值，这使它们变得更加多样化。

## 另请参阅
要了解更多关于函数的信息，请浏览以下内容：

- Arduino 官方函数参考：https://www.arduino.cc/reference/en/language/functions/
- 了解更多关于 DRY 原则：https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
- 关于子程序历史的复习资料：https://en.wikipedia.org/wiki/Subroutine
