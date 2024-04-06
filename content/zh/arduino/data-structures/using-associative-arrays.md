---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:43.820208-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u4E25\u683C\u6765\u8BF4\uFF0CArduino \u6CA1\
  \u6709\u5185\u7F6E\u5BF9\u5173\u8054\u6570\u7EC4\u7684\u652F\u6301\uFF0C\u5982\u540C\
  \u4F60\u5728\u9AD8\u7EA7\u8BED\u8A00\u4E2D\u627E\u5230\u7684\u90A3\u6837\u3002\u4F46\
  \u662F\uFF0C\u4E0D\u8981\u5BB3\u6015\u3002\u6211\u4EEC\u53EF\u4EE5\u5DE7\u5999\u5730\
  \u4F7F\u7528\u7ED3\u6784\u4F53\u548C\u6570\u7EC4\u6765\u6A21\u4EFF\u8FD9\u4E00\u529F\
  \u80FD\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u7B80\u5355\u7684\u4F8B\u5B50\uFF0C\u7528\
  \u4E8E\u521B\u5EFA\u4E00\u4E2A\u7528\u6765\u5B58\u50A8\u548C\u8BBF\u95EE\u4E0D\u540C\
  \u57CE\u5E02\u6E29\u5EA6\u7684\u57FA\u672C\u201C\u5173\u8054\u6570\u7EC4\u201D\u3002\
  \ \u9996\u5148\uFF0C\u5B9A\u4E49\u4E00\u4E2A\u7ED3\u6784\u4F53\u6765\u4FDD\u5B58\
  \u57CE\u5E02\uFF08\u952E\uFF09\u53CA\u5176\u6E29\u5EA6\uFF08\u503C\uFF09\uFF1A."
lastmod: '2024-04-05T22:38:47.211013-06:00'
model: gpt-4-0125-preview
summary: "\u4E25\u683C\u6765\u8BF4\uFF0CArduino \u6CA1\u6709\u5185\u7F6E\u5BF9\u5173\
  \u8054\u6570\u7EC4\u7684\u652F\u6301\uFF0C\u5982\u540C\u4F60\u5728\u9AD8\u7EA7\u8BED\
  \u8A00\u4E2D\u627E\u5230\u7684\u90A3\u6837\u3002\u4F46\u662F\uFF0C\u4E0D\u8981\u5BB3\
  \u6015\u3002\u6211\u4EEC\u53EF\u4EE5\u5DE7\u5999\u5730\u4F7F\u7528\u7ED3\u6784\u4F53\
  \u548C\u6570\u7EC4\u6765\u6A21\u4EFF\u8FD9\u4E00\u529F\u80FD\u3002\u8FD9\u91CC\u6709\
  \u4E00\u4E2A\u7B80\u5355\u7684\u4F8B\u5B50\uFF0C\u7528\u4E8E\u521B\u5EFA\u4E00\u4E2A\
  \u7528\u6765\u5B58\u50A8\u548C\u8BBF\u95EE\u4E0D\u540C\u57CE\u5E02\u6E29\u5EA6\u7684\
  \u57FA\u672C\u201C\u5173\u8054\u6570\u7EC4\u201D\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

## 如何操作:
严格来说，Arduino 没有内置对关联数组的支持，如同你在高级语言中找到的那样。但是，不要害怕。我们可以巧妙地使用结构体和数组来模仿这一功能。这里有一个简单的例子，用于创建一个用来存储和访问不同城市温度的基本“关联数组”。

首先，定义一个结构体来保存城市（键）及其温度（值）：

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

接下来，初始化一个 `CityTemperature` 对象数组：

```cpp
CityTemperature temperatures[] = {
  {"New York", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

以下是如何访问并显示特定城市的温度：

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("洛杉矶的温度是：");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // 目前这里没什么。
}
```

运行这段代码，你会得到以下输出：

```
洛杉矶的温度是：22.0
```

## 深入了解
从历史上看，像 C 和 C++（Arduino 语法衍生自它们）这样的编程语言没有内置关联数组，导致需要上面展示的这种解决方法。这种方法相对简单，但随着数据大小的增加，由于其查找时间为 O(n)，它的扩展性很差。

像 Python 提供的字典和 JavaScript 的对象都是为了这个目的，两者都对于管理键值对更为高效。在 Arduino 中，当性能和效率变得关键时，开发者可能会选择使用通过库实现的更专业的数据结构，如哈希表。

尽管 Arduino 原生不支持关联数组，但社区已经开发了诸如 `HashMap` 的库，可以添加到你的项目中，提供与自己动手做相比更好的性能的类似功能。这些库通常提供了一种更优雅、更高效的管理关联数组的方法，特别是对于更复杂的项目。
