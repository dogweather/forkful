---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:39.756639-07:00
description: "\u65B9\u6CD5\uFF1A Node.js\u3067\u306F\u3001stderr\u3078\u306E\u66F8\
  \u304D\u8FBC\u307F\u306F`console.error()`\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u7528\
  \u3059\u308B\u304B\u3001`process.stderr`\u306B\u76F4\u63A5\u66F8\u304D\u8FBC\u3080\
  \u3053\u3068\u3067\u9054\u6210\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u4E21\
  \u65B9\u306E\u30A2\u30D7\u30ED\u30FC\u30C1\u3092\u793A\u3059\u4F8B\u3092\u793A\u3057\
  \u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.485069-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

## 方法：
Node.jsでは、stderrへの書き込みは`console.error()`メソッドを使用するか、`process.stderr`に直接書き込むことで達成できます。以下に両方のアプローチを示す例を示します：

```javascript
// console.error()を使用
console.error('これはエラーメッセージです。');

// process.stderrに直接書き込む
process.stderr.write('これは別のエラーメッセージです。\n');
```

両方の方法のサンプル出力は、stdoutと混在せずにstderrストリームに表示されます：
```
これはエラーメッセージです。
これは別のエラーメッセージです。
```

より洗練された、またはアプリケーション固有のログ記録のために、多くのJavaScriptプログラマーは`winston`や`bunyan`のようなサードパーティのライブラリを使用します。`winston`を使用した簡単な例を示します：

まず、npm経由で`winston`をインストールします：
```shell
npm install winston
```

次に、`winston`を配置して、エラーをstderrに記録します：
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// エラーメッセージのログ記録
logger.error('エラーがwinstonを通して記録されました。');
```

このセットアップにより、`winston`を使用してエラーをログ記録する場合、stderrに向けられ、標準出力とエラー出力の明確な区分を維持するのに役立ちます。
