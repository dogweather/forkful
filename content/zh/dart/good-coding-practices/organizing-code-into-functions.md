---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:13.574954-07:00
description: "\u5728 Dart \u4E2D\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570\u662F\
  \u6307\u5B9A\u4E49\u53EF\u91CD\u7528\u7684\u4EE3\u7801\u5757\u6765\u6267\u884C\u7279\
  \u5B9A\u4EFB\u52A1\uFF0C\u8FD9\u4E9B\u4EFB\u52A1\u901A\u5E38\u5305\u62EC\u63A5\u6536\
  \u8F93\u5165\u3001\u5904\u7406\u6570\u636E\uFF0C\u5E76\u53EF\u80FD\u8FD4\u56DE\u8F93\
  \u51FA\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u589E\u5F3A\u4EE3\
  \u7801\u7684\u53EF\u8BFB\u6027\u3001\u51CF\u5C11\u91CD\u590D\uFF0C\u5E76\u4FBF\u4E8E\
  \u7EF4\u62A4\uFF0C\u6700\u7EC8\u5BFC\u81F4\u66F4\u6A21\u5757\u5316\u548C\u53EF\u7BA1\
  \u7406\u7684\u4EE3\u7801\u5E93\u3002"
lastmod: '2024-03-11T00:14:21.183889-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570\u662F\
  \u6307\u5B9A\u4E49\u53EF\u91CD\u7528\u7684\u4EE3\u7801\u5757\u6765\u6267\u884C\u7279\
  \u5B9A\u4EFB\u52A1\uFF0C\u8FD9\u4E9B\u4EFB\u52A1\u901A\u5E38\u5305\u62EC\u63A5\u6536\
  \u8F93\u5165\u3001\u5904\u7406\u6570\u636E\uFF0C\u5E76\u53EF\u80FD\u8FD4\u56DE\u8F93\
  \u51FA\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u589E\u5F3A\u4EE3\
  \u7801\u7684\u53EF\u8BFB\u6027\u3001\u51CF\u5C11\u91CD\u590D\uFF0C\u5E76\u4FBF\u4E8E\
  \u7EF4\u62A4\uFF0C\u6700\u7EC8\u5BFC\u81F4\u66F4\u6A21\u5757\u5316\u548C\u53EF\u7BA1\
  \u7406\u7684\u4EE3\u7801\u5E93\u3002"
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u4E3A\u51FD\u6570"
---

{{< edit_this_page >}}

## 什么 & 为什么?
在 Dart 中将代码组织成函数是指定义可重用的代码块来执行特定任务，这些任务通常包括接收输入、处理数据，并可能返回输出。程序员这样做是为了增强代码的可读性、减少重复，并便于维护，最终导致更模块化和可管理的代码库。

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
