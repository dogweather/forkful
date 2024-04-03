---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:40.313093-07:00
description: "\u5728 Dart \u4E2D\u5904\u7406\u9519\u8BEF\u6D89\u53CA\u5230\u9884\u89C1\
  \u548C\u7BA1\u7406\u7A0B\u5E8F\u6267\u884C\u8FC7\u7A0B\u4E2D\u51FA\u73B0\u7684\u5F02\
  \u5E38\uFF0C\u4EE5\u589E\u5F3A\u53EF\u9760\u6027\u548C\u53EF\u7528\u6027\u3002\u7A0B\
  \u5E8F\u5458\u5B9E\u73B0\u9519\u8BEF\u5904\u7406\u662F\u4E3A\u4E86\u9632\u6B62\u7A0B\
  \u5E8F\u5D29\u6E83\u5E76\u5411\u7528\u6237\u63D0\u4F9B\u6709\u610F\u4E49\u7684\u53CD\
  \u9988\uFF0C\u786E\u4FDD\u66F4\u5E73\u6ED1\u3001\u66F4\u5B89\u5168\u7684\u5E94\u7528\
  \u4F53\u9A8C\u3002"
lastmod: '2024-03-13T22:44:47.428359-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u5904\u7406\u9519\u8BEF\u6D89\u53CA\u5230\u9884\u89C1\
  \u548C\u7BA1\u7406\u7A0B\u5E8F\u6267\u884C\u8FC7\u7A0B\u4E2D\u51FA\u73B0\u7684\u5F02\
  \u5E38\uFF0C\u4EE5\u589E\u5F3A\u53EF\u9760\u6027\u548C\u53EF\u7528\u6027\u3002\u7A0B\
  \u5E8F\u5458\u5B9E\u73B0\u9519\u8BEF\u5904\u7406\u662F\u4E3A\u4E86\u9632\u6B62\u7A0B\
  \u5E8F\u5D29\u6E83\u5E76\u5411\u7528\u6237\u63D0\u4F9B\u6709\u610F\u4E49\u7684\u53CD\
  \u9988\uFF0C\u786E\u4FDD\u66F4\u5E73\u6ED1\u3001\u66F4\u5B89\u5168\u7684\u5E94\u7528\
  \u4F53\u9A8C\u3002."
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
