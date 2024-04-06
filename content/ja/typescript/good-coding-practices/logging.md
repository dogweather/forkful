---
date: 2024-01-26 01:10:02.743602-07:00
description: "\u65B9\u6CD5\uFF1A TypeScript\u3067\u306F\u3001\u30B3\u30F3\u30BD\u30FC\
  \u30EB\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3057\u305F\u57FA\u672C\u7684\u306A\
  \u30ED\u30B0\u8A18\u9332\u3092\u7C21\u5358\u306B\u5B9F\u88C5\u3059\u308B\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3057\u3001`winston`\u3084`pino`\u306E\u3088\u3046\
  \u306A\u9AD8\u5EA6\u306A\u30ED\u30B0\u8A18\u9332\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\
  \u7D71\u5408\u3059\u308B\u3053\u3068\u3082\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\
  \u306B`console.log`\u3092\u4F7F\u7528\u3057\u305F\u57FA\u672C\u7684\u306A\u4F8B\u3068\
  \u3001`winston`\u3092\u4F7F\u7528\u3057\u305F\u3088\u308A\u9AD8\u5EA6\u306A\u4F8B\
  \u3092\u793A\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T22:37:50.063407-06:00'
model: gpt-4-1106-preview
summary: "\u65B9\u6CD5\uFF1A TypeScript\u3067\u306F\u3001\u30B3\u30F3\u30BD\u30FC\u30EB\
  \u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3057\u305F\u57FA\u672C\u7684\u306A\u30ED\
  \u30B0\u8A18\u9332\u3092\u7C21\u5358\u306B\u5B9F\u88C5\u3059\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3057\u3001`winston`\u3084`pino`\u306E\u3088\u3046\u306A\
  \u9AD8\u5EA6\u306A\u30ED\u30B0\u8A18\u9332\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u7D71\
  \u5408\u3059\u308B\u3053\u3068\u3082\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306B\
  `console.log`\u3092\u4F7F\u7528\u3057\u305F\u57FA\u672C\u7684\u306A\u4F8B\u3068\u3001\
  `winston`\u3092\u4F7F\u7528\u3057\u305F\u3088\u308A\u9AD8\u5EA6\u306A\u4F8B\u3092\
  \u793A\u3057\u307E\u3059\u3002"
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

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
