---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:40.313093-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Dart \u652F\u6301\u4E24\u79CD\u7C7B\u578B\u7684\
  \u9519\u8BEF\uFF1A*\u7F16\u8BD1\u65F6*\u9519\u8BEF\u548C*\u8FD0\u884C\u65F6*\u9519\
  \u8BEF\u3002\u7F16\u8BD1\u65F6\u9519\u8BEF\u5728\u4EE3\u7801\u8FD0\u884C\u4E4B\u524D\
  \u7531 Dart \u5206\u6790\u5668\u68C0\u6D4B\u5230\uFF0C\u800C\u8FD0\u884C\u65F6\u9519\
  \u8BEF\uFF0C\u6216\u8005\u8BF4\u5F02\u5E38\uFF0C\u5728\u6267\u884C\u8FC7\u7A0B\u4E2D\
  \u53D1\u751F\u3002\u4EE5\u4E0B\u662F\u5728 Dart \u4E2D\u5904\u7406\u5F02\u5E38\u7684\
  \u65B9\u6CD5\uFF1A \u4F7F\u7528 `try-catch`\u6765\u6355\u83B7\u5F02\u5E38\uFF0C\u9632\
  \u6B62\u5B83\u4EEC\u5D29\u6E83\u4F60\u7684\u5E94\u7528\uFF1A."
lastmod: '2024-03-13T22:44:47.428359-06:00'
model: gpt-4-0125-preview
summary: "Dart \u652F\u6301\u4E24\u79CD\u7C7B\u578B\u7684\u9519\u8BEF\uFF1A*\u7F16\
  \u8BD1\u65F6*\u9519\u8BEF\u548C*\u8FD0\u884C\u65F6*\u9519\u8BEF\u3002\u7F16\u8BD1\
  \u65F6\u9519\u8BEF\u5728\u4EE3\u7801\u8FD0\u884C\u4E4B\u524D\u7531 Dart \u5206\u6790\
  \u5668\u68C0\u6D4B\u5230\uFF0C\u800C\u8FD0\u884C\u65F6\u9519\u8BEF\uFF0C\u6216\u8005\
  \u8BF4\u5F02\u5E38\uFF0C\u5728\u6267\u884C\u8FC7\u7A0B\u4E2D\u53D1\u751F\u3002\u4EE5\
  \u4E0B\u662F\u5728 Dart \u4E2D\u5904\u7406\u5F02\u5E38\u7684\u65B9\u6CD5\uFF1A\n\
  \n\u4F7F\u7528 `try-catch`\u6765\u6355\u83B7\u5F02\u5E38\uFF0C\u9632\u6B62\u5B83\
  \u4EEC\u5D29\u6E83\u4F60\u7684\u5E94\u7528\uFF1A."
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 如何操作:
Dart 支持两种类型的错误：*编译时*错误和*运行时*错误。编译时错误在代码运行之前由 Dart 分析器检测到，而运行时错误，或者说异常，在执行过程中发生。以下是在 Dart 中处理异常的方法：

### Try-Catch
使用 `try-catch`来捕获异常，防止它们崩溃你的应用：

```dart
try {
  var result = 100 ~/ 0; // 尝试除以零，抛出异常
} catch (e) {
  print('捕获到一个异常：$e'); // 处理异常
}
```
示例输出：`捕获到一个异常：IntegerDivisionByZeroException`

### 特定异常
要处理特定异常，在 `catch` 之后声明异常类型：

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('不能除以零。'); // 特定地处理除以零的异常
}
```
示例输出：`不能除以零。`

### 堆栈追踪
要获取调试用的堆栈追踪，在 catch 块中使用第二个参数：

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('异常：$e');
  print('堆栈追踪：$s'); // 打印堆栈追踪用于调试
}
```

### Finally
无论是否抛出异常，都使用 `finally` 在 try/catch 之后执行代码：

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('捕获到一个异常：$e');
} finally {
  print('这总是被执行。'); // 清理代码或最后的步骤
}
```
示例输出：
```
捕获到一个异常：IntegerDivisionByZeroException
这总是被执行。
```

### 第三方库
尽管 Dart 的核心库对于错误处理非常健壮，但你也可以使用第三方包如 `dartz` 进行函数式编程，它引入了像 `Either` 和 `Option` 这样可以用于错误处理的概念。以下是使用 `dartz` 进行错误处理的示例：

1. 在你的 `pubspec.yaml` 文件的依赖项下添加 `dartz`：
```yaml
dependencies:
  dartz: ^0.10.0
```

2. 在你的 Dart 代码中使用 `Either` 以优雅地处理错误：
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividend, int divisor) {
  if (divisor == 0) {
    return Left('不能除以零。');
  } else {
    return Right(dividend ~/ divisor);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('错误：$left'), 
    (right) => print('结果：$right')
  );
}
```
示例输出：`错误：不能除以零。`

`Left` 部分通常代表错误，而 `Right` 部分代表成功。这种模式允许以更函数式的方式处理错误，提供了关于错误管理的清晰度和控制力。
