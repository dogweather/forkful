---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:16.120274-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:15.834928-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
---

{{< edit_this_page >}}

## 何となぜ？

PHPにおいて標準エラー（stderr）への書き込みは、エラーメッセージや診断を標準出力（stdout）とは別に管理し、開発者がデバッグやログ取得のための出力ストリームをよりよく管理できるようにすることについてです。プログラマーはこの技術を利用して、エラーメッセージがプログラムの出力と干渉しないようにし、アプリケーションの監視とトラブルシューティングを容易にします。

## 方法

PHPでは、`fwrite()`関数とあらかじめ定義された定数`STDERR`を使用することで、stderrへの書き込みを実現できます。これは、エラー出力ストリームを代表しています。

```php
<?php
// stderrへのシンプルなメッセージの書き込み。
fwrite(STDERR, "これはエラーメッセージです。\n");
```

コマンドラインからスクリプトを実行したときのサンプル出力:
```
これはエラーメッセージです。
```

より実践的な使用法を示すために、ユーザー入力を解析して予期しないデータに遭遇したシナリオを考えてみましょう:
```php
<?php
$input = '予期しないデータ';

// ユーザー入力のエラー処理のシミュレーション。
if ($input === '予期しないデータ') {
    fwrite(STDERR, "エラー: 予期しない入力を受け取りました。\n");
    exit(1); // エラーを示すためにゼロ以外の値で終了します。
}
```

PHPの組み込みのstderr処理機能は一般的に十分ですが、より複雑なアプリケーションを扱う際や、外部システムとのstderrログを統合したい場合、Monologのようなサードパーティのライブラリが強力な助けとなります。Monologは、stderrはもちろんのこと、ファイルやソケットなど、多くの対象に対応したログライブラリです。

Monologを使用してstderrに書き込む方法:

まず、Composerを介してMonologがインストールされていることを確認してください:
```
composer require monolog/monolog
```

次に、`php://stderr`をターゲットとした`StreamHandler`を使用するようにMonologを設定します:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// ログチャンネルの作成
$log = new Logger('name');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// 警告メッセージをstderrにログとして追加
$log->warning('これは警告メッセージです。');
```

上記のコードは、詳細なログ設定や外部のログ監視が必要なアプリケーションに特に有用な、Monologを利用してstderrに警告メッセージを送信します。
