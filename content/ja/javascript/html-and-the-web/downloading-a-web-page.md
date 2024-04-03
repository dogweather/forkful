---
date: 2024-01-20 17:44:50.418874-07:00
description: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3059\u308B\u3063\u3066\uFF1F\u305D\u308C\u306F\u3001\u30A6\u30A7\u30D6\u304B\
  \u3089\u60C5\u5831\u3092\u53D6\u5F97\u3057\u3001\u4F7F\u3048\u308B\u3088\u3046\u306B\
  \u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u306A\u305C\u305D\u308C\u3092\u3059\u308B\u306E\uFF1F\u30C7\u30FC\u30BF\u5206\u6790\
  \u3001\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3001\u30AA\u30D5\u30E9\u30A4\u30F3\
  \u95B2\u89A7\u3001\u3042\u308B\u3044\u306F\u5185\u5BB9\u306E\u30D0\u30C3\u30AF\u30A2\
  \u30C3\u30D7\u306E\u305F\u3081\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.675363-06:00'
model: gpt-4-1106-preview
summary: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3059\u308B\u3063\u3066\uFF1F\u305D\u308C\u306F\u3001\u30A6\u30A7\u30D6\u304B\
  \u3089\u60C5\u5831\u3092\u53D6\u5F97\u3057\u3001\u4F7F\u3048\u308B\u3088\u3046\u306B\
  \u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u306A\u305C\u305D\u308C\u3092\u3059\u308B\u306E\uFF1F\u30C7\u30FC\u30BF\u5206\u6790\
  \u3001\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3001\u30AA\u30D5\u30E9\u30A4\u30F3\
  \u95B2\u89A7\u3001\u3042\u308B\u3044\u306F\u5185\u5BB9\u306E\u30D0\u30C3\u30AF\u30A2\
  \u30C3\u30D7\u306E\u305F\u3081\u3067\u3059\u3002."
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## What & Why? (何となぜ？)
ウェブページをダウンロードするって？それは、ウェブから情報を取得し、使えるようにすることです。プログラマーはなぜそれをするの？データ分析、スクレイピング、オフライン閲覧、あるいは内容のバックアップのためです。

## How to: (やり方)
JavaScriptでウェブページをダウンロードする一番の方法は、`fetch`関数を使うことです。以下が簡単な例です。

```javascript
// Node.jsの場合
const fetch = require('node-fetch');

async function downloadPage(url) {
  try {
    const response = await fetch(url);
    const pageBody = await response.text();
    console.log(pageBody);
  } catch (error) {
    console.error('Download failed:', error);
  }
}

downloadPage('https://example.com'); // ここにダウンロードしたいウェブページのURLを入れます
```

次は出力の一部です。

```
<!doctype html>
<html>
...
</html>
```

## Deep Dive (深掘り)
ウェブページのダウンロードは、古くからウェブ開発の基本機能です。古い方法には、XMLHttpRequestやiframeの使用がありましたが、現在は`fetch`APIが標準でこれに取って代わっています。`fetch`はプロミスを返し、より現代的な非同期制御を提供します。他にも、`Axios`や`Got`などのサードパーティのライブラリがあり、追加の機能や簡単なAPIを提供しています。ただし、これらを使うには追加のセットアップが必要です。

適切なエラーハンドリングが重要であり、特にネットワーク問題やHTTP応答コードを見落とさないよう気をつけましょう。また、大量のダウンロードを実行する場合は、ウェブサイトの利用規約を確認し、レートリミットやロボット排除標準に従う必要があります。

## See Also (参照)
- MDN Web Docsの`fetch`API: [Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- `node-fetch`ライブラリ: [node-fetch](https://www.npmjs.com/package/node-fetch)
- JavaScript非同期処理の詳細: [Asynchronous programming](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous)
- `Axios`ライブラリ: [Axios on GitHub](https://github.com/axios/axios)
- レートリミットとロボット排除標準について: [Robots exclusion standard](https://en.wikipedia.org/wiki/Robots_exclusion_standard)
