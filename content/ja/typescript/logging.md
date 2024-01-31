---
title:                "ロギング"
date:                  2024-01-26T01:10:02.743602-07:00
model:                 gpt-4-1106-preview
simple_title:         "ロギング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/logging.md"
---

{{< edit_this_page >}}

## 何となぜ？

ログ記録とは、プログラムの実行中に生じるイベント、エラー、その他の重要な情報を外部媒体（多くの場合はファイルやデータベース）に記録するプロセスです。プログラマーは、ソフトウェアの挙動を監視したり、問題をデバッグしたり、セキュリティやパフォーマンス分析のためにシステム活動を追跡するためにログを使用します。

## 方法：

TypeScriptでは、コンソールメソッドを使用した基本的なログ記録を簡単に実装することができますし、`winston`や`pino`のような高度なログ記録ライブラリを統合することもできます。以下に`console.log`を使用した基本的な例と、`winston`を使用したより高度な例を示します。

```TypeScript
// 基本的なコンソールログ記録
console.log('情報: アプリケーションの起動...');
console.error('エラー: データの取得に失敗しました。');

// サンプル出力
// 情報: アプリケーションの起動...
// エラー: データの取得に失敗しました。
```

より堅牢なログ記録のために、`winston`を設定しましょう：

```TypeScript
import { createLogger, format, transports } from 'winston';

const logger = createLogger({
  level: 'info',
  format: format.combine(
    format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    format.printf(info => `${info.timestamp} ${info.level}: ${info.message}`)
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'combined.log' })
  ]
});

logger.info('サーバーが起動しました！');
logger.warn('ディスクスペース不足の警告。');
logger.error('データベースへの接続に失敗しました。');

// combined.log でのサンプル出力
// 2023-01-20 14:42:07 情報: サーバーが起動しました！
// 2023-01-20 14:42:09 警告: ディスクスペース不足の警告。
// 2023-01-20 14:42:12 エラー: データベースへの接続に失敗しました。
```

## 詳細解説：

コンピューティングの文脈におけるログの概念は、プログラミングの初期に遡ります。その用語自体は、海事における記録保持システムである「logbook」に由来しています。歴史的には、プログラムイベントは特にメインフレーム時代に、物理的なプリントアウトやターミナル出力にしばしばログされました。

今日に至るまで、単純なテキストファイルから複雑なログ管理システムに至るまで、さまざまなログ記録ニーズに対応するツールやライブラリがたくさん存在しています。`winston`の代替品としては、高性能を売りにしている`pino`や、JSONベースの`Bunyan`などがあります。Node.jsを使用する際、ログライブラリは多くの場合、ログを異なる宛先に導くストリーム機構を提供し、ログローテーションのサポートやカスタムフォーマッタを備えています。

実装の観点からは、ログメッセージには通常、タイムスタンプ、重大度レベル（info、warn、errorなど）、そして実際のメッセージが含まれます。良いログ記録の実践には、ログレベルを適切に分類すること、ログに機密データを含めないこと、高スループットアプリケーションにおけるパフォーマンスへの影響を検討することが推奨されます。

## 参照：

- [Winston - ほぼすべてのものに対応するロガー](https://www.npmjs.com/package/winston)
- [Pino - 非常に低オーバーヘッドなNode.jsロガー](https://www.npmjs.com/package/pino)
- [Node.js ログ記録のベストプラクティス](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [12要素アプリ - ログ](https://12factor.net/logs)
