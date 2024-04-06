---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:16.120274-07:00
description: "\u65B9\u6CD5 PHP\u3067\u306F\u3001`fwrite()`\u95A2\u6570\u3068\u3042\
  \u3089\u304B\u3058\u3081\u5B9A\u7FA9\u3055\u308C\u305F\u5B9A\u6570`STDERR`\u3092\
  \u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u3001stderr\u3078\u306E\u66F8\u304D\u8FBC\
  \u307F\u3092\u5B9F\u73FE\u3067\u304D\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30A8\
  \u30E9\u30FC\u51FA\u529B\u30B9\u30C8\u30EA\u30FC\u30E0\u3092\u4EE3\u8868\u3057\u3066\
  \u3044\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:43.119934-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

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
