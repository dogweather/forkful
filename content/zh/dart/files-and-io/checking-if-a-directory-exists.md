---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:26.258098-07:00
description: "\u5728Dart\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u662F\
  \u6307\u5728\u6587\u4EF6\u7CFB\u7EDF\u4E0A\u7684\u6307\u5B9A\u8DEF\u5F84\u4E0A\u9A8C\
  \u8BC1\u76EE\u5F55\u7684\u5B58\u5728\u4E0E\u5426\uFF0C\u4EE5\u4FBF\u5728\u8BFB\u53D6\
  \u6216\u5199\u5165\u6587\u4EF6\u4E4B\u524D\u8FDB\u884C\u64CD\u4F5C\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u907F\u514D\u5728\u5C1D\u8BD5\u8BBF\u95EE\
  \u6216\u4FEE\u6539\u4E0D\u5B58\u5728\u7684\u76EE\u5F55\u65F6\u53D1\u751F\u9519\u8BEF\
  \u3002"
lastmod: '2024-03-13T22:44:47.437898-06:00'
model: gpt-4-0125-preview
summary: "\u5728Dart\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u662F\u6307\
  \u5728\u6587\u4EF6\u7CFB\u7EDF\u4E0A\u7684\u6307\u5B9A\u8DEF\u5F84\u4E0A\u9A8C\u8BC1\
  \u76EE\u5F55\u7684\u5B58\u5728\u4E0E\u5426\uFF0C\u4EE5\u4FBF\u5728\u8BFB\u53D6\u6216\
  \u5199\u5165\u6587\u4EF6\u4E4B\u524D\u8FDB\u884C\u64CD\u4F5C\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u907F\u514D\u5728\u5C1D\u8BD5\u8BBF\u95EE\u6216\
  \u4FEE\u6539\u4E0D\u5B58\u5728\u7684\u76EE\u5F55\u65F6\u53D1\u751F\u9519\u8BEF\u3002"
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

## 什么和为什么?

在Dart中检查目录是否存在是指在文件系统上的指定路径上验证目录的存在与否，以便在读取或写入文件之前进行操作。程序员这样做是为了避免在尝试访问或修改不存在的目录时发生错误。

## 如何操作:

Dart使用`dart:io`库来处理文件和目录。这是检查目录是否存在的一个简单方法：

```dart
import 'dart:io';

void main() {
  var directory = Directory('path/to/your/directory');

  if (directory.existsSync()) {
    print('目录存在');
  } else {
    print('目录不存在');
  }
}
```
如果目录确实存在的示例输出：
```
目录存在
```

或者，如果不存在：
```
目录不存在
```

要处理更复杂的场景，比如异步检查或如果目录不存在则创建目录，你可以使用以下方法：

```dart
import 'dart:io';

void main() async {
  var directory = Directory('path/to/your/directory');

  // 异步检查目录是否存在
  var exists = await directory.exists();
  if (exists) {
    print('目录存在');
  } else {
    print('目录不存在，正在创建...');
    await directory.create(); // 这会创建目录
    print('目录已创建');
  }
}
```

如果目录不存在并且被创建的示例输出：
```
目录不存在，正在创建...
目录已创建
```

Dart的内置功能通常足以处理文件和目录，因此通常不需要第三方库来完成这项任务。然而，对于更复杂的文件系统操作，像`path`这样的包（以一种平台无关的方式操作路径）可以补充`dart:io`库，但不直接提供比所展示的更高级的目录存在性检查。
