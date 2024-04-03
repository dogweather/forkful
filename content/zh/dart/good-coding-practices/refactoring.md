---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:56.642928-07:00
description: "\u5728 Dart \u4E2D\u7684\u91CD\u6784\u662F\u6307\u5728\u4E0D\u6539\u53D8\
  \u73B0\u6709\u4EE3\u7801\u5916\u90E8\u884C\u4E3A\u7684\u60C5\u51B5\u4E0B\uFF0C\u91CD\
  \u65B0\u7EC4\u7EC7\u73B0\u6709\u4EE3\u7801\u7684\u8FC7\u7A0B\uFF0C\u65E8\u5728\u6539\
  \u5584\u5176\u5185\u90E8\u7ED3\u6784\u3001\u53EF\u8BFB\u6027\u548C\u53EF\u7EF4\u62A4\
  \u6027\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u8FDB\u884C\u91CD\u6784\uFF0C\u4EE5\u4F7F\
  \u4EE3\u7801\u66F4\u6E05\u6670\u3001\u66F4\u6613\u4E8E\u7406\u89E3\u6216\u66F4\u9AD8\
  \u6548\uFF0C\u4ECE\u800C\u4FC3\u8FDB\u672A\u6765\u4FEE\u6539\u7684\u4FBF\u5229\u6027\
  \u5E76\u964D\u4F4E\u51FA\u73B0\u9519\u8BEF\u7684\u53EF\u80FD\u6027\u3002"
lastmod: '2024-03-13T22:44:47.429770-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u7684\u91CD\u6784\u662F\u6307\u5728\u4E0D\u6539\u53D8\
  \u73B0\u6709\u4EE3\u7801\u5916\u90E8\u884C\u4E3A\u7684\u60C5\u51B5\u4E0B\uFF0C\u91CD\
  \u65B0\u7EC4\u7EC7\u73B0\u6709\u4EE3\u7801\u7684\u8FC7\u7A0B\uFF0C\u65E8\u5728\u6539\
  \u5584\u5176\u5185\u90E8\u7ED3\u6784\u3001\u53EF\u8BFB\u6027\u548C\u53EF\u7EF4\u62A4\
  \u6027\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u8FDB\u884C\u91CD\u6784\uFF0C\u4EE5\u4F7F\
  \u4EE3\u7801\u66F4\u6E05\u6670\u3001\u66F4\u6613\u4E8E\u7406\u89E3\u6216\u66F4\u9AD8\
  \u6548\uFF0C\u4ECE\u800C\u4FC3\u8FDB\u672A\u6765\u4FEE\u6539\u7684\u4FBF\u5229\u6027\
  \u5E76\u964D\u4F4E\u51FA\u73B0\u9519\u8BEF\u7684\u53EF\u80FD\u6027\u3002."
title: "\u91CD\u6784"
weight: 19
---

## 什么 & 为什么？

在 Dart 中的重构是指在不改变现有代码外部行为的情况下，重新组织现有代码的过程，旨在改善其内部结构、可读性和可维护性。程序员经常进行重构，以使代码更清晰、更易于理解或更高效，从而促进未来修改的便利性并降低出现错误的可能性。

## 如何操作：

### 示例 1：重命名和提取方法

重构前，您可能有一段代码混合了不同层次的抽象或职责，比如计算折扣然后应用它：

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("最终价格：$finalPrice");
}
```

**输出：**
```
最终价格：80.0
```

重构后，您可以将折扣计算提取到它自己的方法中，并给它一个有意义的名称：

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = calculateFinalPrice(price, discount);
  print("最终价格：$finalPrice");
}

double calculateFinalPrice(double price, double discount) {
  return price - (price * discount);
}
```

**输出：**
```
最终价格：80.0
```

通过将计算提取到一个方法中，您现在有了一个清晰定义的操作，可以被重用、独立测试，并且容易修改。

### 示例 2：简化条件表达式

重构前，条件语句可能过于复杂或难以阅读：

```dart
void main() {
  var customerType = "regular";
  double discount;
  
  if (customerType == "regular") {
    discount = 0.05;
  } else if (customerType == "member") {
    discount = 0.1;
  } else {
    discount = 0.0;
  }

  print("折扣：$discount");
}
```

**输出：**
```
折扣：0.05
```

重构后，考虑使用映射（map）来获得更清晰的结构，以及便于更新或扩展客户类型和折扣：

```dart
void main() {
  var customerType = "regular";
  var discounts = {
    "regular": 0.05,
    "member": 0.1,
    "none": 0.0,
  };

  var discount = discounts[customerType] ?? 0.0;
  print("折扣：$discount");
}
```

**输出：**
```
折扣：0.05
```

这种重构不仅使代码更简洁，而且以一种更易于理解和维护的方式封装了确定折扣的逻辑。

### 第三方库进行重构

在 Dart 中进行重构，尤其是在 Flutter 应用程序中，[Dart DevTools](https://dart.dev/tools/dart-devtools) 套件非常宝贵。它包括性能工具、小部件检查器和源级调试器。虽然 Dart DevTools 不是第三方库，但它经常与像 `flutter_bloc` 这样的库一起使用，以便以利于重构来清晰地管理状态，从而提高模块化和可读性。遗憾的是，由于本条目的范围，这里不会提供使用第三方库的具体代码示例，但鼓励开发人员探索这些工具，以增强他们在 Dart/Flutter 应用程序中的重构过程。
