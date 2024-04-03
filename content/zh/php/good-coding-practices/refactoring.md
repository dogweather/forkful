---
date: 2024-01-26 01:57:55.872748-07:00
description: "\u91CD\u6784\u662F\u5BF9\u73B0\u6709\u8BA1\u7B97\u673A\u4EE3\u7801\u7684\
  \u7ED3\u6784\u8FDB\u884C\u91CD\u65B0\u8C03\u6574\u800C\u4E0D\u6539\u53D8\u5176\u5916\
  \u90E8\u884C\u4E3A\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\u6784\
  \u4EE5\u6539\u5584\u8F6F\u4EF6\u7684\u975E\u529F\u80FD\u5C5E\u6027\uFF0C\u4F7F\u4EE3\
  \u7801\u66F4\u6E05\u6670\u3001\u66F4\u9AD8\u6548\uFF0C\u4E14\u66F4\u6613\u4E8E\u7EF4\
  \u62A4\u3002"
lastmod: '2024-03-13T22:44:47.873249-06:00'
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u5BF9\u73B0\u6709\u8BA1\u7B97\u673A\u4EE3\u7801\u7684\
  \u7ED3\u6784\u8FDB\u884C\u91CD\u65B0\u8C03\u6574\u800C\u4E0D\u6539\u53D8\u5176\u5916\
  \u90E8\u884C\u4E3A\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\u6784\
  \u4EE5\u6539\u5584\u8F6F\u4EF6\u7684\u975E\u529F\u80FD\u5C5E\u6027\uFF0C\u4F7F\u4EE3\
  \u7801\u66F4\u6E05\u6670\u3001\u66F4\u9AD8\u6548\uFF0C\u4E14\u66F4\u6613\u4E8E\u7EF4\
  \u62A4\u3002."
title: "\u91CD\u6784\u4EE3\u7801"
weight: 19
---

## 什么和为什么？
重构是对现有计算机代码的结构进行重新调整而不改变其外部行为的过程。程序员进行重构以改善软件的非功能属性，使代码更清晰、更高效，且更易于维护。

## 如何操作：
让我们拿一个典型的PHP代码片段，并对其应用一些重构魔法。

在重构之前，我们的代码可能看起来像这样：

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Item: " . $item['name'];
        echo " - Price: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Total: " . array_sum(array_column($order, 'price'));
    }
}
```

但我们可以重构这段代码以提高其清晰度和模块化：

```php
function printItem($item) {
    echo "Item: {$item['name']} - Price: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Total: " . calculateTotal($order);
    }
}
```
通过将`printOrderDetails`函数细分为较小的函数，我们的代码变得更加可读并且更易于调试。

## 深入探讨
重构起源于20世纪90年代初的Smalltalk编程社区，并通过马丁·福勒的开创性著作《重构：改善既有代码的设计》（1999年）得到了进一步的推广。虽然重构可以应用于任何编程语言，但PHP的动态特性提供了一些独特的挑战和机会。

重构的替代方案可能包括从头开始重写代码，这通常更具风险且耗时更多。在PHP生态系统中，像PHPStan和Rector这样的工具可以分别自动发现并执行一些重构操作。在实施方面，保持重构规模小且通过单元测试进行广泛测试是确保成功重构而不引入缺陷的关键实践。

## 参见
- 马丁·福勒的《重构》书籍：https://martinfowler.com/books/refactoring.html
- PHPStan，一个PHP静态分析工具：https://phpstan.org/
- Rector，一个用于PHP代码自动重构的工具：https://getrector.org/
- 使用PHPUnit进行PHP单元测试：https://phpunit.de/
