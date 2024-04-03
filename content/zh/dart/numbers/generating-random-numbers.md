---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:37.181099-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Dart \u7684\u6838\u5FC3\u5E93\u652F\u6301\
  \u4F7F\u7528 `dart:math` \u4E2D\u627E\u5230\u7684 `Random` \u7C7B\u6765\u751F\u6210\
  \u968F\u673A\u6570\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\uFF1A\
  ."
lastmod: '2024-03-13T22:44:47.412797-06:00'
model: gpt-4-0125-preview
summary: "Dart \u7684\u6838\u5FC3\u5E93\u652F\u6301\u4F7F\u7528 `dart:math` \u4E2D\
  \u627E\u5230\u7684 `Random` \u7C7B\u6765\u751F\u6210\u968F\u673A\u6570\u3002\u8FD9\
  \u91CC\u6709\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\uFF1A."
title: "\u751F\u6210\u968F\u673A\u6570"
weight: 12
---

## 如何操作：
Dart 的核心库支持使用 `dart:math` 中找到的 `Random` 类来生成随机数。这里有一个基本示例：

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // 生成一个 0 到 99 之间的随机整数
  double randomDouble = rand.nextDouble(); // 生成一个 0.0 到 1.0 之间的随机双精度浮点数
  print(randomNumber);
  print(randomDouble);
}
```

*示例输出：（每次运行都会变化）*

```
23
0.6722390975465775
```

对于需要加密随机性的用例，Dart 提供了 `Random.secure` 构造函数：

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*示例输出：（每次运行都会变化）*

```
45
```

如果你在处理 Flutter 项目或需要更复杂的随机性，你可能会发现 `faker` 包对于生成广泛的随机数据（如名字、地址和日期）很有用。

要使用 `faker`，首先，在你的 `pubspec.yaml` 文件中添加它：

```yaml
dependencies:
  faker: ^2.0.0
```

然后，按照如下方式导入并使用它：

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // 生成一个随机名字
  print(faker.address.city()); // 生成一个随机城市名
}
```

*示例输出：*

```
Josie Runolfsdottir
East Lysanne
```
