---
date: 2024-01-26 01:07:58.418812-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.254105-06:00'
model: gpt-4-1106-preview
summary: "\u30ED\u30B0\u3068\u306F\u57FA\u672C\u7684\u306B\u3001\u30B3\u30FC\u30C9\
  \u306E\u305F\u3081\u306E\u65E5\u8A18\u3092\u3064\u3051\u308B\u3088\u3046\u306A\u3082\
  \u306E\u3067\u3059\u3002\u305D\u308C\u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u304C\u5B9F\u884C\u3055\u308C\u308B\u3068\u304D\u306B\u767A\u751F\u3059\
  \u308B\u30A4\u30D9\u30F3\u30C8\u3001\u30A8\u30E9\u30FC\u3001\u305D\u306E\u4ED6\u306E\
  \u91CD\u8981\u306A\u30C7\u30FC\u30BF\u30DD\u30A4\u30F3\u30C8\u3092\u8A18\u9332\u3059\
  \u308B\u884C\u70BA\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u5185\u90E8\u3067\u4F55\u304C\u8D77\u3053\u3063\u3066\u3044\u308B\u306E\u304B\u8FFD\
  \u8DE1\u3057\u3001\u554F\u984C\u3092\u30C7\u30D0\u30C3\u30B0\u3057\u3001\u5F8C\u3067\
  \u5206\u6790\u3084\u30B3\u30F3\u30D7\u30E9\u30A4\u30A2\u30F3\u30B9\u306E\u76EE\u7684\
  \u3067\u76E3\u67FB\u306E\u8A3C\u8DE1\u3092\u7DAD\u6301\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
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
