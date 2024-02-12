---
title:                "インタラクティブシェル（REPL）の使用"
aliases: - /ja/php/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:16:46.124090-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ？
インタラクティブシェルまたはREPL（Read-Eval-Print Loop）を使用すると、PHPコードをその場で記述して実行できます。これは、フルスクリプトを作成する手間をかけずにスニペットをテストできるため、実験、デバッグ、もしくは学習に理想的です。

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
