---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:11.544790-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Dart \u63D0\u4F9B\u4E86\u4E00\u79CD\u76F4\u63A5\
  \u7684\u8BED\u6CD5\u6765\u521B\u5EFA\u548C\u64CD\u4F5C Maps\u3002\u4E0B\u9762\u662F\
  \u4E00\u4E9B\u6F14\u793A\u57FA\u672C\u64CD\u4F5C\u5982\u521B\u5EFA\u3001\u6DFB\u52A0\
  \u5143\u7D20\u548C\u83B7\u53D6\u503C\u7684\u4F8B\u5B50\u3002"
lastmod: '2024-03-13T22:44:47.409116-06:00'
model: gpt-4-0125-preview
summary: "Dart \u63D0\u4F9B\u4E86\u4E00\u79CD\u76F4\u63A5\u7684\u8BED\u6CD5\u6765\u521B\
  \u5EFA\u548C\u64CD\u4F5C Maps\u3002\u4E0B\u9762\u662F\u4E00\u4E9B\u6F14\u793A\u57FA\
  \u672C\u64CD\u4F5C\u5982\u521B\u5EFA\u3001\u6DFB\u52A0\u5143\u7D20\u548C\u83B7\u53D6\
  \u503C\u7684\u4F8B\u5B50."
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

## 如何操作:
Dart 提供了一种直接的语法来创建和操作 Maps。下面是一些演示基本操作如创建、添加元素和获取值的例子。

```dart
void main() {
  // 创建一个 map
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple'
  };

  // 添加一个新的键值对
  fruitColors['orange'] = 'orange';

  // 通过其键访问值
  print(fruitColors['apple']); // 输出：red

  // 更新值
  fruitColors['banana'] = 'green';

  // 遍历 Map
  fruitColors.forEach((fruit, color) {
    print('$fruit: $color');
  });
  // 示例输出：
  // apple: red
  // banana: green
  // grape: purple
  // orange: orange
}
```

对于复杂的数据结构或扩展的功能，Dart 程序员经常依赖于附加的库。其中一个库是 `collection`，它提供了高级集合类型和实用工具。尽管 `collection` 不改变处理 Maps 的基本方式，但它用实用函数和更复杂的集合类型丰富了它们。以下是您如何使用它来执行更具体的任务，如按其值对 Map 进行排序：

首先，确保 `collection` 包包含在您的 `pubspec.yaml` 文件中：

```yaml
dependencies:
  collection: ^1.15.0
```

然后，您可以按如下方式使用它：

```dart
import 'package:collection/collection.dart';

void main() {
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple',
    'orange': 'orange'
  };

  // 按其值（颜色）对 Map 进行排序
  var sortedFruitsByColor = SplayTreeMap.from(
    fruitColors,
    (key1, key2) => fruitColors[key1]!.compareTo(fruitColors[key2]!)
  );

  print(sortedFruitsByColor);
  // 输出：
  // {orange: orange, apple: red, banana: yellow, grape: purple}
}
```

这个示例演示了基于它们的值对 Map 的条目进行排序，展示了 Dart 及其活跃的生态系统如何灵活地处理关联数组以进行更复杂的数据操作。
