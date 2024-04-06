---
date: 2024-01-26 01:07:58.418812-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A PHP\u306B\u306F\u4F7F\u3044\u3084\
  \u3059\u3044\u7D44\u307F\u8FBC\u307F\u306E\u30A8\u30E9\u30FC\u30ED\u30B0\u6A5F\u80FD\
  \u304C\u3042\u308A\u307E\u3059\u3002\u30B3\u30FC\u30C9\u306B `error_log()` \u3092\
  \u633F\u5165\u3059\u308B\u3060\u3051\u3067\u3001\u30B5\u30FC\u30D0\u30FC\u30ED\u30B0\
  \u306B\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u9001\u4FE1\u3067\u304D\u307E\u3059\u3002\
  \u7279\u5B9A\u306E\u30D5\u30A1\u30A4\u30EB\u306B\u66F8\u304D\u8FBC\u3080\u3088\u3046\
  \u306B\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u3059\u308B\u3053\u3068\u3082\u3067\u304D\
  \u307E\u3059\u3002"
lastmod: '2024-04-05T22:38:41.783714-06:00'
model: gpt-4-1106-preview
summary: "\u3069\u3046\u3084\u3063\u3066\uFF1A PHP\u306B\u306F\u4F7F\u3044\u3084\u3059\
  \u3044\u7D44\u307F\u8FBC\u307F\u306E\u30A8\u30E9\u30FC\u30ED\u30B0\u6A5F\u80FD\u304C\
  \u3042\u308A\u307E\u3059\u3002\u30B3\u30FC\u30C9\u306B `error_log()` \u3092\u633F\
  \u5165\u3059\u308B\u3060\u3051\u3067\u3001\u30B5\u30FC\u30D0\u30FC\u30ED\u30B0\u306B\
  \u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u9001\u4FE1\u3067\u304D\u307E\u3059\u3002\u7279\
  \u5B9A\u306E\u30D5\u30A1\u30A4\u30EB\u306B\u66F8\u304D\u8FBC\u3080\u3088\u3046\u306B\
  \u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u3059\u308B\u3053\u3068\u3082\u3067\u304D\u307E\
  \u3059\u3002"
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

## どうやって：
PHPには使いやすい組み込みのエラーログ機能があります。コードに `error_log()` を挿入するだけで、サーバーログにメッセージを送信できます。特定のファイルに書き込むようにカスタマイズすることもできます。

```php
<?php
// 簡単な情報メッセージをログに記録
error_log("This is an info log entry.");

// エラーメッセージをログに記録
error_log("This is an error log entry.", 0);

// 指定したファイルにログを記録
file_put_contents('/path/to/your/custom.log', "A custom log entry.\n", FILE_APPEND);

// 構造化ログのためのMonologを使用
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// ロガーを作成
$logger = new Logger('name');
// ハンドラをいくつか追加
$logger->pushHandler(new StreamHandler('/path/to/your/monolog.log', Logger::WARNING));

// これでロガーを使用できます
$logger->warning('This is a warning log!');
$logger->error('This is an error log!');
?>
```

これにより、ログがサーバーログまたは指定したファイルにプレーンテキスト形式で出力されます。

## 深堀り：
歴史的には、PHP開発者は `error_log()` 関数やApache/Nginxのログを使って問題を捉えていましたが、プレーンテキストファイルを解析する必要があり、フィルタリングやソートの簡単な方法がないため混沌としていました。そこにMonologのようなログライブラリが導入され、PHPにおける構造化ロギングの時代が到来しました。これらのソリューションは、複数のログチャンネル、重大度レベル、フォーマットされた出力（プログラム的に解析するのに夢のようなJSONなど）を提供することで、より良いコントロールを提供します。

Monologの代替には、Log4php、KLogger、ApacheのLog4phpなどがあります。実装の面では、データをどこにでもダンプするだけでなく、ログローテーション、アーカイブ戦略、監視ツールとの統合などを考慮することが、本当に役立つロギングには必要です。

[PSR-3 ロガーインターフェース](https://www.php-fig.org/psr/psr-3/)を念頭に置くべきであり、これはログライブラリの共通インターフェースを概説しており、相互運用性とログメカニズムへの一貫したアクセス方法を保証しています。

## 参照：
- [Monolog GitHub リポジトリ](https://github.com/Seldaek/monolog)
- [PSR-3 ロガーインターフェース仕様](https://www.php-fig.org/psr/psr-3/)
- [PHPエラーログドキュメント](https://www.php.net/manual/en/function.error-log.php)
- [KLogger: PHP用シンプルなロギングクラス](https://github.com/katzgrau/KLogger)
- [Log4php: PHP用の多機能ロギングフレームワーク](https://logging.apache.org/log4php/)

組み込みの関数で練習を始めるのも良いでしょうが、よりメンテナンス性と拡張性のあるアプローチを求めるなら、Monologのようなライブラリに慣れる時間を投資することを検討してください。ハッピーロギング！
