---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:13.574954-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u5728 Dart \u4E2D\uFF0C\u5982\u679C\u51FD\
  \u6570\u4E0D\u8FD4\u56DE\u503C\uFF0C\u5219\u4F7F\u7528 `void` \u5173\u952E\u5B57\
  \u5B9A\u4E49\u51FD\u6570\uFF0C\u5426\u5219\u6307\u5B9A\u5B83\u8FD4\u56DE\u503C\u7684\
  \u7C7B\u578B\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u7B80\u5355\u7684\u51FD\u6570\uFF0C\
  \u7528\u4E8E\u6253\u5370\u95EE\u5019\u6D88\u606F\uFF1A."
lastmod: '2024-03-13T22:44:47.425732-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\uFF0C\u5982\u679C\u51FD\u6570\u4E0D\u8FD4\u56DE\u503C\
  \uFF0C\u5219\u4F7F\u7528 `void` \u5173\u952E\u5B57\u5B9A\u4E49\u51FD\u6570\uFF0C\
  \u5426\u5219\u6307\u5B9A\u5B83\u8FD4\u56DE\u503C\u7684\u7C7B\u578B\u3002\u8FD9\u91CC\
  \u6709\u4E00\u4E2A\u7B80\u5355\u7684\u51FD\u6570\uFF0C\u7528\u4E8E\u6253\u5370\u95EE\
  \u5019\u6D88\u606F\uFF1A."
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u4E3A\u51FD\u6570"
weight: 18
---

## 如何操作:


### 基础函数
在 Dart 中，如果函数不返回值，则使用 `void` 关键字定义函数，否则指定它返回值的类型。这里有一个简单的函数，用于打印问候消息：

```dart
void greet(String name) {
  print('Hello, $name!');
}

void main() {
  greet('Alice');  // 输出: Hello, Alice!
}
```

### 返回值
函数可以返回值。下面的例子接收两个整数作为输入，并返回它们的和：

```dart
int add(int a, int b) {
  return a + b;
}

void main() {
  var sum = add(5, 3);
  print(sum);  // 输出: 8
}
```

### 匿名函数
Dart 支持匿名函数（也称为 lambda 表达式或闭包），这对于简短的即时功能很方便。这是如何在列表的 `forEach` 方法中使用匿名函数的示例：

```dart
void main() {
  var fruits = ['apple', 'banana', 'cherry'];
  fruits.forEach((item) {
    print(item);
  });
  // 输出:
  // apple
  // banana
  // cherry
}
```

### 单表达式函数的箭头语法
对于只包含单个表达式的函数，Dart 提供了使用“箭头”符号 (`=>`) 的简洁语法。这对于简短的函数或将函数作为参数传递特别有用：

```dart
int square(int num) => num * num;

void main() {
  print(square(4));  // 输出: 16
}
```

### 使用第三方库
对于更复杂或特殊的功能，Dart 程序员通常依赖于第三方库。考虑使用 `http` 库进行 HTTP 请求。首先，在你的 pubspec.yaml 文件下的依赖项中添加 `http`：

```
dependencies:
  http: ^0.13.3
```

然后，你可以使用它从网络上获取数据：

```dart
import 'package:http/http.dart' as http;

Future<void> fetchUserData() async {
  var response = await http.get(Uri.parse('https://api.example.com/users/1'));
  print(response.body);
}

void main() {
  fetchUserData();
  // 预期输出：用户的 JSON 数据。实际输出将取决于 API 的响应。
}
```

记住，当你将 Dart 代码组织成函数时，要考虑可重用性、清晰度和单一职责原则。这不仅使你的代码更简洁，而且也使其更容易被其他人（和未来的你）理解和维护。
