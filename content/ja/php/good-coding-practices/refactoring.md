---
date: 2024-01-26 01:50:00.497653-07:00
description: "\u65B9\u6CD5\uFF1A \u5178\u578B\u7684\u306APHP\u30B9\u30CB\u30DA\u30C3\
  \u30C8\u3092\u53D6\u308A\u3001\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u30DE\
  \u30B8\u30C3\u30AF\u3092\u9069\u7528\u3057\u3066\u307F\u307E\u3057\u3087\u3046\u3002\
  \ \u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u524D\u3001\u79C1\u305F\u3061\
  \u306E\u30B3\u30FC\u30C9\u306F\u3053\u306E\u3088\u3046\u306B\u898B\u3048\u308B\u304B\
  \u3082\u3057\u308C\u307E\u305B\u3093\uFF1A."
lastmod: '2024-04-05T21:53:43.109394-06:00'
model: gpt-4-0125-preview
summary: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u524D\u3001\u79C1\u305F\
  \u3061\u306E\u30B3\u30FC\u30C9\u306F\u3053\u306E\u3088\u3046\u306B\u898B\u3048\u308B\
  \u304B\u3082\u3057\u308C\u307E\u305B\u3093\uFF1A."
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
weight: 19
---

## 方法：
典型的なPHPスニペットを取り、リファクタリングマジックを適用してみましょう。

リファクタリング前、私たちのコードはこのように見えるかもしれません：

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

しかし、このコードをリファクタリングして、その明瞭さとモジュラリティを向上させることができます：

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
`printOrderDetails`関数をより小さな関数に分割することで、コードがより読みやすく、デバッグしやすくなります。

## ディープダイブ
リファクタリングは、1990年代初頭のsmalltalkプログラミングコミュニティにその起源を持ち、マーティン・ファウラーの画期的な本「Refactoring: Improving the Design of Existing Code」（1999年）によってさらに普及しました。リファクタリングは任意のプログラミング言語に適用できますが、PHPの動的な性質はいくつかのユニークな課題と機会を提供します。

リファクタリングの代替手段には、ゼロからのコードの書き直しが含まれるかもしれませんが、これはしばしばリスクが高く、時間がかかります。PHPエコシステムでは、PHPStanやRectorのようなツールがそれぞれ自動的にいくつかのリファクタリング操作を見つけて実行できます。実装面では、リファクタリングを小さく保ち、単体テストで広範にテストすることが、バグを導入せずに成功したリファクタリングを保証するための重要な実践です。

## 関連項目
- マーティン・ファウラーのリファクタリングの本：https://martinfowler.com/books/refactoring.html
- PHP静的分析ツールPHPStan：https://phpstan.org/
- PHPコードの自動リファクタリングツールRector：https://getrector.org/
- PHPUnitを使用したPHPユニットテスト：https://phpunit.de/
