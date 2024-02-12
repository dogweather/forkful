---
title:                "ロギング"
aliases:
- /ja/php/logging/
date:                  2024-01-26T01:07:58.418812-07:00
model:                 gpt-4-1106-preview
simple_title:         "ロギング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/logging.md"
---

{{< edit_this_page >}}

## 何となぜ？

ログとは基本的に、コードのための日記をつけるようなものです。それは、アプリケーションが実行されるときに発生するイベント、エラー、その他の重要なデータポイントを記録する行為です。プログラマーは、内部で何が起こっているのか追跡し、問題をデバッグし、後で分析やコンプライアンスの目的で監査の証跡を維持するためにこれを行います。

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
