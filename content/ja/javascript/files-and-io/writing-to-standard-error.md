---
aliases:
- /ja/javascript/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:39.756639-07:00
description: "\u2026"
lastmod: 2024-02-18 23:08:55.284117
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
---

{{< edit_this_page >}}

## 何となぜ？
JavaScriptで標準エラー(stderr)への書き込みは、エラーメッセージや重要な情報を特定の別のストリームに向けることで、ログやデバッグ目的でUnix系環境で特に役立ちます。プログラマーは、通常のプログラム出力とエラーメッセージを区別するためにこれを行い、出力管理をより清潔にし、エラー監視を容易にします。

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
