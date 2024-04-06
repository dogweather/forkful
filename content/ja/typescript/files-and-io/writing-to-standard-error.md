---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:55.021319-07:00
description: "\u65B9\u6CD5\uFF1A TypeScript\u306FJavaScript\u306E\u30B9\u30FC\u30D1\
  \u30FC\u30BB\u30C3\u30C8\u3067\u3042\u308B\u305F\u3081\u3001stderr\u3078\u306E\u66F8\
  \u304D\u8FBC\u307F\u306B\u306F\u57FA\u76E4\u3068\u306A\u308BJS\u306E\u30E9\u30F3\
  \u30BF\u30A4\u30E0\u74B0\u5883\uFF08Node.js\u306A\u3069\uFF09\u306B\u4F9D\u5B58\u3057\
  \u3066\u3044\u307E\u3059\u3002\u3053\u308C\u304C\u76F4\u63A5\u884C\u3046\u65B9\u6CD5\
  \u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:37:50.075536-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A TypeScript\u306FJavaScript\u306E\u30B9\u30FC\u30D1\u30FC\
  \u30BB\u30C3\u30C8\u3067\u3042\u308B\u305F\u3081\u3001stderr\u3078\u306E\u66F8\u304D\
  \u8FBC\u307F\u306B\u306F\u57FA\u76E4\u3068\u306A\u308BJS\u306E\u30E9\u30F3\u30BF\
  \u30A4\u30E0\u74B0\u5883\uFF08Node.js\u306A\u3069\uFF09\u306B\u4F9D\u5B58\u3057\u3066\
  \u3044\u307E\u3059\u3002\u3053\u308C\u304C\u76F4\u63A5\u884C\u3046\u65B9\u6CD5\u3067\
  \u3059\uFF1A."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

## 方法：
TypeScriptはJavaScriptのスーパーセットであるため、stderrへの書き込みには基盤となるJSのランタイム環境（Node.jsなど）に依存しています。これが直接行う方法です：

```typescript
console.error("これはエラーメッセージです。");
```

stderrへのサンプル出力：
```
これはエラーメッセージです。
```

Node.js環境では、より低レベルの書き込みに`process.stderr.write()`メソッドも使用できます：

```typescript
process.stderr.write("低レベルのエラーメッセージ。\n");
```

stderrへのサンプル出力：
```
低レベルのエラーメッセージ。
```

より構造化されたエラーロギングには、`winston`や`pino`などの人気のサードパーティライブラリを使用することがあります。ここでは、`winston`を使ってエラーをログする方法を紹介します：

まず、`winston`をインストールします：

```bash
npm install winston
```

次に、TypeScriptファイルでそれを使用します：

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('winstonを使用してログされたエラー。');
```

これにより、エラーはコンソールと`error.log`という名前のファイルの両方に書き込まれます。ファイルに書き込む場合、ディスクスペースの使用に関連する問題を防ぐために、ファイルの権限とロールオーバーを管理することが重要です。
