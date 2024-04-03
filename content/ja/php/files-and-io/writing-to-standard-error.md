---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:16.120274-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.269249-06:00'
model: gpt-4-0125-preview
summary: "PHP\u306B\u304A\u3044\u3066\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\
  \u3078\u306E\u66F8\u304D\u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\
  \u30FC\u30B8\u3084\u8A3A\u65AD\u3092\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\u3068\
  \u306F\u5225\u306B\u7BA1\u7406\u3057\u3001\u958B\u767A\u8005\u304C\u30C7\u30D0\u30C3\
  \u30B0\u3084\u30ED\u30B0\u53D6\u5F97\u306E\u305F\u3081\u306E\u51FA\u529B\u30B9\u30C8\
  \u30EA\u30FC\u30E0\u3092\u3088\u308A\u3088\u304F\u7BA1\u7406\u3067\u304D\u308B\u3088\
  \u3046\u306B\u3059\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u306E\u6280\u8853\u3092\u5229\u7528\u3057\
  \u3066\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u304C\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u306E\u51FA\u529B\u3068\u5E72\u6E09\u3057\u306A\u3044\u3088\u3046\u306B\
  \u3057\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u76E3\u8996\u3068\
  \u30C8\u30E9\u30D6\u30EB\u30B7\u30E5\u30FC\u30C6\u30A3\u30F3\u30B0\u3092\u5BB9\u6613\
  \u306B\u3057\u307E\u3059\u3002."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

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
