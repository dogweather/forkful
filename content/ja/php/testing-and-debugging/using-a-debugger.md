---
date: 2024-01-26 03:50:56.217672-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:15.820027-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガは、プログラマーが実行中のコードが実際に何をしているかを理解するのに役立つツールです。これは、プログラムがクラッシュしたり、間違った答えを吐き出したりする原因となる厄介な問題—バグ—に焦点を当てて潰すことを可能にする拡大鏡です。デバッガを使用する理由は、何時間ものprintステートメントと推測ゲームを節約できるからです。

## 使用方法：
PHPにはXdebugというインタラクティブなデバッガが付属しています。以下がその使用方法です。

まず、`php.ini` ファイルにXdebugがインストールされ、設定されていることを確認してください：

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

次に、バグを含むシンプルなPHPスクリプトを書きます：

```PHP
<?php
function add($a, $b) {
    return $a - $b; // おっと！これはマイナスではなくプラスであるべきです
}

$result = add(1, 2);
echo "結果は: $result"; // 出力は3であるべきですが、-1です
```

PhpStormのようなIDEを使用して、行番号の隣をクリックしてブレークポイントを設定します。デバッガを実行し、実行をステップごとに進めると変数がどのように変化するかを観察します。`add`関数をステップオーバーすると、`$result`が-1になることに気づくでしょう。これは予期せぬことです。

## 深堀り：
歴史的に、PHPは主に小さなスクリプト用に使われ、デバッグはコード全体に`var_dump()`や`print_r()`ステートメントを追加することで行われていました。時間が経つにつれて、PHPがウェブ開発の重要なプレイヤーとなるにつれ、XdebugやZend Debuggerのようなより洗練されたツールが使用され始めました。

Xdebugの代替としてpcovやphpdbgがあります。これらはさまざまな機能を提供しますが、Xdebugほど全機能を備えているわけではないかもしれません。phpdbgは軽量でPHP専用のデバッガであり、PHP 5.6以降にPHPと共に配布されています。pcovはコードカバレッジドライバです。

デバッガを実装する際には、セキュリティの脆弱性を露呈させる可能性があり、パフォーマンスを低下させるため、本番サーバーでデバッガをオンにしておくべきではないことを忘れないでください。

## 参照：
- [Xdebugドキュメント](https://xdebug.org/docs/)
- [PhpStormデバッグガイド](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net の phpdbg について](https://www.php.net/manual/ja/book.phpdbg.php)
- [pcov on GitHub](https://github.com/krakjoe/pcov)
