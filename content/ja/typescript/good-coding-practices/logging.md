---
date: 2024-01-26 01:10:02.743602-07:00
description: "\u30ED\u30B0\u8A18\u9332\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u306E\u5B9F\u884C\u4E2D\u306B\u751F\u3058\u308B\u30A4\u30D9\u30F3\u30C8\u3001\u30A8\
  \u30E9\u30FC\u3001\u305D\u306E\u4ED6\u306E\u91CD\u8981\u306A\u60C5\u5831\u3092\u5916\
  \u90E8\u5A92\u4F53\uFF08\u591A\u304F\u306E\u5834\u5408\u306F\u30D5\u30A1\u30A4\u30EB\
  \u3084\u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\uFF09\u306B\u8A18\u9332\u3059\u308B\u30D7\
  \u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u306E\u6319\u52D5\u3092\u76E3\u8996\u3057\u305F\
  \u308A\u3001\u554F\u984C\u3092\u30C7\u30D0\u30C3\u30B0\u3057\u305F\u308A\u3001\u30BB\
  \u30AD\u30E5\u30EA\u30C6\u30A3\u3084\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u5206\
  \u6790\u306E\u305F\u3081\u306B\u30B7\u30B9\u30C6\u30E0\u6D3B\u52D5\u3092\u8FFD\u8DE1\
  \u3059\u308B\u305F\u3081\u306B\u30ED\u30B0\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.767819-06:00'
model: gpt-4-1106-preview
summary: "\u30ED\u30B0\u8A18\u9332\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u306E\u5B9F\u884C\u4E2D\u306B\u751F\u3058\u308B\u30A4\u30D9\u30F3\u30C8\u3001\u30A8\
  \u30E9\u30FC\u3001\u305D\u306E\u4ED6\u306E\u91CD\u8981\u306A\u60C5\u5831\u3092\u5916\
  \u90E8\u5A92\u4F53\uFF08\u591A\u304F\u306E\u5834\u5408\u306F\u30D5\u30A1\u30A4\u30EB\
  \u3084\u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\uFF09\u306B\u8A18\u9332\u3059\u308B\u30D7\
  \u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u306E\u6319\u52D5\u3092\u76E3\u8996\u3057\u305F\
  \u308A\u3001\u554F\u984C\u3092\u30C7\u30D0\u30C3\u30B0\u3057\u305F\u308A\u3001\u30BB\
  \u30AD\u30E5\u30EA\u30C6\u30A3\u3084\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u5206\
  \u6790\u306E\u305F\u3081\u306B\u30B7\u30B9\u30C6\u30E0\u6D3B\u52D5\u3092\u8FFD\u8DE1\
  \u3059\u308B\u305F\u3081\u306B\u30ED\u30B0\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\
  ."
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

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
