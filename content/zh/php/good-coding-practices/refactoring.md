---
date: 2024-01-26 01:57:55.872748-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8BA9\u6211\u4EEC\u62FF\u4E00\u4E2A\u5178\
  \u578B\u7684PHP\u4EE3\u7801\u7247\u6BB5\uFF0C\u5E76\u5BF9\u5176\u5E94\u7528\u4E00\
  \u4E9B\u91CD\u6784\u9B54\u6CD5\u3002 \u5728\u91CD\u6784\u4E4B\u524D\uFF0C\u6211\u4EEC\
  \u7684\u4EE3\u7801\u53EF\u80FD\u770B\u8D77\u6765\u50CF\u8FD9\u6837\uFF1A."
lastmod: '2024-04-05T21:53:48.180768-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u91CD\u6784\u4E4B\u524D\uFF0C\u6211\u4EEC\u7684\u4EE3\u7801\u53EF\
  \u80FD\u770B\u8D77\u6765\u50CF\u8FD9\u6837\uFF1A."
title: "\u91CD\u6784\u4EE3\u7801"
weight: 19
---

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
