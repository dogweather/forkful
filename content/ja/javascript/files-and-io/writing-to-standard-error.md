---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:39.756639-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.698058-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u3067\u6A19\u6E96\u30A8\u30E9\u30FC(stderr)\u3078\u306E\u66F8\
  \u304D\u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\
  \u91CD\u8981\u306A\u60C5\u5831\u3092\u7279\u5B9A\u306E\u5225\u306E\u30B9\u30C8\u30EA\
  \u30FC\u30E0\u306B\u5411\u3051\u308B\u3053\u3068\u3067\u3001\u30ED\u30B0\u3084\u30C7\
  \u30D0\u30C3\u30B0\u76EE\u7684\u3067Unix\u7CFB\u74B0\u5883\u3067\u7279\u306B\u5F79\
  \u7ACB\u3061\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u901A\
  \u5E38\u306E\u30D7\u30ED\u30B0\u30E9\u30E0\u51FA\u529B\u3068\u30A8\u30E9\u30FC\u30E1\
  \u30C3\u30BB\u30FC\u30B8\u3092\u533A\u5225\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u3001\u51FA\u529B\u7BA1\u7406\u3092\u3088\u308A\u6E05\u6F54\u306B\
  \u3057\u3001\u30A8\u30E9\u30FC\u76E3\u8996\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\
  \u3002."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

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
