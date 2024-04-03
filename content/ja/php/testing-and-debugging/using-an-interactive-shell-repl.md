---
date: 2024-01-26 04:16:46.124090-07:00
description: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u307E\
  \u305F\u306FREPL\uFF08Read-Eval-Print Loop\uFF09\u3092\u4F7F\u7528\u3059\u308B\u3068\
  \u3001PHP\u30B3\u30FC\u30C9\u3092\u305D\u306E\u5834\u3067\u8A18\u8FF0\u3057\u3066\
  \u5B9F\u884C\u3067\u304D\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30D5\u30EB\u30B9\
  \u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u624B\u9593\u3092\u304B\u3051\
  \u305A\u306B\u30B9\u30CB\u30DA\u30C3\u30C8\u3092\u30C6\u30B9\u30C8\u3067\u304D\u308B\
  \u305F\u3081\u3001\u5B9F\u9A13\u3001\u30C7\u30D0\u30C3\u30B0\u3001\u3082\u3057\u304F\
  \u306F\u5B66\u7FD2\u306B\u7406\u60F3\u7684\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:42.248657-06:00'
model: gpt-4-0125-preview
summary: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u307E\
  \u305F\u306FREPL\uFF08Read-Eval-Print Loop\uFF09\u3092\u4F7F\u7528\u3059\u308B\u3068\
  \u3001PHP\u30B3\u30FC\u30C9\u3092\u305D\u306E\u5834\u3067\u8A18\u8FF0\u3057\u3066\
  \u5B9F\u884C\u3067\u304D\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30D5\u30EB\u30B9\
  \u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u624B\u9593\u3092\u304B\u3051\
  \u305A\u306B\u30B9\u30CB\u30DA\u30C3\u30C8\u3092\u30C6\u30B9\u30C8\u3067\u304D\u308B\
  \u305F\u3081\u3001\u5B9F\u9A13\u3001\u30C7\u30D0\u30C3\u30B0\u3001\u3082\u3057\u304F\
  \u306F\u5B66\u7FD2\u306B\u7406\u60F3\u7684\u3067\u3059\u3002."
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
