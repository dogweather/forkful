---
title:                "リファクタリング"
date:                  2024-01-26T01:50:00.497653-07:00
model:                 gpt-4-0125-preview
simple_title:         "リファクタリング"

category:             "PHP"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/refactoring.md"
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
