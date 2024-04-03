---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:55.021319-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.781960-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\u3067\u306F\u3001\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\
  \u306B\u66F8\u304D\u8FBC\u3080\u3053\u3068\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\
  \u30BB\u30FC\u30B8\u3084\u30ED\u30B0\u3092\u74B0\u5883\u306E\u30A8\u30E9\u30FC\u51FA\
  \u529B\u30B9\u30C8\u30EA\u30FC\u30E0\uFF08\u4F8B\u3048\u3070\u3001node."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

## 何となぜ？
TypeScriptでは、標準エラー（stderr）に書き込むことは、エラーメッセージやログを環境のエラー出力ストリーム（例えば、node.jsやWebブラウザのコンソール）に直接送信するプロセスです。これは、通常プログラムデータに使用される標準出力（stdout）と干渉せずに、問題の診断を確実に行うために不可欠です。これにより、エラー処理とロギングが効率的かつ一貫して管理されます。

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
