---
title:                "使用关联数组"
date:                  2024-01-30T19:12:43.820208-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"

category:             "Arduino"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?
在 Arduino 的世界里，关联数组允许你将键与值配对，有点像将袜子与它们的另一半配对一样。当你需要使用描述性的名称来存储和检索数据时，它们成为了首选，能让你的代码更清晰、更易于理解。

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
