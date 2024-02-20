---
date: 2024-01-26 01:09:10.009895-07:00
description: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570\u610F\u5473\u7740\u5C06\
  \u4EE3\u7801\u5206\u89E3\u4E3A\u53EF\u91CD\u7528\u7684\u4EE3\u7801\u5757\uFF0C\u6BCF\
  \u4E2A\u4EE3\u7801\u5757\u6267\u884C\u7279\u5B9A\u7684\u4EFB\u52A1\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4F7F\u4EE3\u7801\u66F4\u6613\u4E8E\u9605\
  \u8BFB\u3001\u8C03\u8BD5\u548C\u91CD\u7528\u3002\u8FD9\u5C31\u50CF\u5C06\u4E50\u9AD8\
  \u79EF\u6728\u5206\u7C7B\u653E\u5165\u4E0D\u540C\u7684\u7BB1\u5B50\u4E2D - \u5B83\
  \u53EF\u4EE5\u9632\u6B62\u4F60\u6BCF\u6B21\u60F3\u8981\u6784\u5EFA\u4E1C\u897F\u65F6\
  \u90FD\u8981\u4ECE\u4E00\u5806\u6DF7\u4E71\u7684\u79EF\u6728\u4E2D\u7FFB\u627E\u3002"
lastmod: 2024-02-19 22:05:07.122366
model: gpt-4-1106-preview
summary: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570\u610F\u5473\u7740\u5C06\
  \u4EE3\u7801\u5206\u89E3\u4E3A\u53EF\u91CD\u7528\u7684\u4EE3\u7801\u5757\uFF0C\u6BCF\
  \u4E2A\u4EE3\u7801\u5757\u6267\u884C\u7279\u5B9A\u7684\u4EFB\u52A1\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4F7F\u4EE3\u7801\u66F4\u6613\u4E8E\u9605\
  \u8BFB\u3001\u8C03\u8BD5\u548C\u91CD\u7528\u3002\u8FD9\u5C31\u50CF\u5C06\u4E50\u9AD8\
  \u79EF\u6728\u5206\u7C7B\u653E\u5165\u4E0D\u540C\u7684\u7BB1\u5B50\u4E2D - \u5B83\
  \u53EF\u4EE5\u9632\u6B62\u4F60\u6BCF\u6B21\u60F3\u8981\u6784\u5EFA\u4E1C\u897F\u65F6\
  \u90FD\u8981\u4ECE\u4E00\u5806\u6DF7\u4E71\u7684\u79EF\u6728\u4E2D\u7FFB\u627E\u3002"
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
---

{{< edit_this_page >}}

## 什么和为什么？
将代码组织成函数意味着将代码分解为可重用的代码块，每个代码块执行特定的任务。程序员这样做是为了使代码更易于阅读、调试和重用。这就像将乐高积木分类放入不同的箱子中 - 它可以防止你每次想要构建东西时都要从一堆混乱的积木中翻找。

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
