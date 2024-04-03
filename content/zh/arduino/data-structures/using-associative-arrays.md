---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:43.820208-07:00
description: "\u5728 Arduino \u7684\u4E16\u754C\u91CC\uFF0C\u5173\u8054\u6570\u7EC4\
  \u5141\u8BB8\u4F60\u5C06\u952E\u4E0E\u503C\u914D\u5BF9\uFF0C\u6709\u70B9\u50CF\u5C06\
  \u889C\u5B50\u4E0E\u5B83\u4EEC\u7684\u53E6\u4E00\u534A\u914D\u5BF9\u4E00\u6837\u3002\
  \u5F53\u4F60\u9700\u8981\u4F7F\u7528\u63CF\u8FF0\u6027\u7684\u540D\u79F0\u6765\u5B58\
  \u50A8\u548C\u68C0\u7D22\u6570\u636E\u65F6\uFF0C\u5B83\u4EEC\u6210\u4E3A\u4E86\u9996\
  \u9009\uFF0C\u80FD\u8BA9\u4F60\u7684\u4EE3\u7801\u66F4\u6E05\u6670\u3001\u66F4\u6613\
  \u4E8E\u7406\u89E3\u3002"
lastmod: '2024-03-13T22:44:48.054655-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Arduino \u7684\u4E16\u754C\u91CC\uFF0C\u5173\u8054\u6570\u7EC4\u5141\
  \u8BB8\u4F60\u5C06\u952E\u4E0E\u503C\u914D\u5BF9\uFF0C\u6709\u70B9\u50CF\u5C06\u889C\
  \u5B50\u4E0E\u5B83\u4EEC\u7684\u53E6\u4E00\u534A\u914D\u5BF9\u4E00\u6837\u3002\u5F53\
  \u4F60\u9700\u8981\u4F7F\u7528\u63CF\u8FF0\u6027\u7684\u540D\u79F0\u6765\u5B58\u50A8\
  \u548C\u68C0\u7D22\u6570\u636E\u65F6\uFF0C\u5B83\u4EEC\u6210\u4E3A\u4E86\u9996\u9009\
  \uFF0C\u80FD\u8BA9\u4F60\u7684\u4EE3\u7801\u66F4\u6E05\u6670\u3001\u66F4\u6613\u4E8E\
  \u7406\u89E3\u3002."
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
