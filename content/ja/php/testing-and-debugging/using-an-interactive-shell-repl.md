---
date: 2024-01-26 04:16:46.124090-07:00
description: "\u65B9\u6CD5\uFF1A PHP REPL\u3092\u8D77\u52D5\u3059\u308B\u306B\u306F\
  \u3001\u30BF\u30FC\u30DF\u30CA\u30EB\u3067`php -a`\u3092\u5B9F\u884C\u3057\u307E\
  \u3059\u3002\u4EE5\u4E0B\u306F\u305D\u306E\u52D5\u4F5C\u306E\u4E00\u4F8B\u3067\u3059\
  \uFF1A."
lastmod: '2024-04-05T22:38:41.777907-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A PHP REPL\u3092\u8D77\u52D5\u3059\u308B\u306B\u306F\u3001\
  \u30BF\u30FC\u30DF\u30CA\u30EB\u3067`php -a`\u3092\u5B9F\u884C\u3057\u307E\u3059\
  \u3002\u4EE5\u4E0B\u306F\u305D\u306E\u52D5\u4F5C\u306E\u4E00\u4F8B\u3067\u3059\uFF1A\
  ."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

## 方法：
PHP REPLを起動するには、ターミナルで`php -a`を実行します。以下はその動作の一例です：

```php
php > echo "Hello, World!";
Hello, World!
php > $arr = [1, 2, 3];
php > print_r($arr);
配列
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

関数の定義も可能です：

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## より深く
REPLは、1960年代のLISPの初期からある形で存在しています。PHPのインタラクティブシェルは、PythonやJavaScriptのような言語のものに比べて進んでいないです。セッション間で状態を保持しない、オートコンプリートのような機能が欠けています。より機能豊富なPHP REPLを求める場合は、`psysh`や`boris`のような代替品を検討してください。これらのサードパーティ製シェルは、より優れた内省ツール、タブ補完、さらにデバッガーを提供します。

PHPのREPLの内部では、入力された各行のコードをコンパイルして実行することで動作します。このアプローチの限界は、同じセッション内でクラスの再宣言などが不可能な点にあります。単純なテストには最適ですが、複雑なタスクでは扱いにくくなります。

## 関連するもの
- [PHP マニュアル - インタラクティブシェル](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: PHP用のランタイム開発者コンソール、インタラクティブデバッガー及びREPL](https://psysh.org/)
- [Boris: PHP用の小さなREPL](https://github.com/borisrepl/boris)
