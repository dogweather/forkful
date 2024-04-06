---
date: 2024-01-26 03:50:56.217672-07:00
description: "\u4F7F\u7528\u65B9\u6CD5\uFF1A PHP\u306B\u306FXdebug\u3068\u3044\u3046\
  \u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u306A\u30C7\u30D0\u30C3\u30AC\u304C\
  \u4ED8\u5C5E\u3057\u3066\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u304C\u305D\u306E\u4F7F\
  \u7528\u65B9\u6CD5\u3067\u3059\u3002 \u307E\u305A\u3001`php.ini` \u30D5\u30A1\u30A4\
  \u30EB\u306BXdebug\u304C\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3055\u308C\u3001\u8A2D\
  \u5B9A\u3055\u308C\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3066\u304F\
  \u3060\u3055\u3044\uFF1A."
lastmod: '2024-04-05T22:38:41.781278-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u7528\u65B9\u6CD5\uFF1A PHP\u306B\u306FXdebug\u3068\u3044\u3046\u30A4\
  \u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u306A\u30C7\u30D0\u30C3\u30AC\u304C\u4ED8\
  \u5C5E\u3057\u3066\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u304C\u305D\u306E\u4F7F\u7528\
  \u65B9\u6CD5\u3067\u3059\u3002 \u307E\u305A\u3001`php.ini` \u30D5\u30A1\u30A4\u30EB\
  \u306BXdebug\u304C\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3055\u308C\u3001\u8A2D\u5B9A\
  \u3055\u308C\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3066\u304F\u3060\
  \u3055\u3044\uFF1A."
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
weight: 35
---

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
