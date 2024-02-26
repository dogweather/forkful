---
date: 2024-01-26 01:50:00.497653-07:00
description: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u306F\u3001\u5916\u90E8\
  \u306E\u52D5\u4F5C\u3092\u5909\u3048\u305A\u306B\u65E2\u5B58\u306E\u30B3\u30F3\u30D4\
  \u30E5\u30FC\u30BF\u30B3\u30FC\u30C9\u306E\u69CB\u9020\u3092\u518D\u69CB\u7BC9\u3059\
  \u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u884C\u3044\u3001\u30BD\
  \u30D5\u30C8\u30A6\u30A7\u30A2\u306E\u975E\u6A5F\u80FD\u5C5E\u6027\u3092\u6539\u5584\
  \u3057\u3066\u3001\u30B3\u30FC\u30C9\u3092\u3088\u308A\u30AF\u30EA\u30A2\u3067\u52B9\
  \u7387\u7684\u304B\u3064\u4FDD\u5B88\u3057\u3084\u3059\u304F\u3057\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.255244-07:00'
model: gpt-4-0125-preview
summary: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u306F\u3001\u5916\u90E8\
  \u306E\u52D5\u4F5C\u3092\u5909\u3048\u305A\u306B\u65E2\u5B58\u306E\u30B3\u30F3\u30D4\
  \u30E5\u30FC\u30BF\u30B3\u30FC\u30C9\u306E\u69CB\u9020\u3092\u518D\u69CB\u7BC9\u3059\
  \u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u884C\u3044\u3001\u30BD\
  \u30D5\u30C8\u30A6\u30A7\u30A2\u306E\u975E\u6A5F\u80FD\u5C5E\u6027\u3092\u6539\u5584\
  \u3057\u3066\u3001\u30B3\u30FC\u30C9\u3092\u3088\u308A\u30AF\u30EA\u30A2\u3067\u52B9\
  \u7387\u7684\u304B\u3064\u4FDD\u5B88\u3057\u3084\u3059\u304F\u3057\u307E\u3059\u3002"
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
---

{{< edit_this_page >}}

## 何となぜ？
リファクタリングは、外部の動作を変えずに既存のコンピュータコードの構造を再構築するプロセスです。プログラマーはリファクタリングを行い、ソフトウェアの非機能属性を改善して、コードをよりクリアで効率的かつ保守しやすくします。

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
