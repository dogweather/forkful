---
date: 2024-01-20 17:44:50.418874-07:00
description: "How to: (\u3084\u308A\u65B9) JavaScript\u3067\u30A6\u30A7\u30D6\u30DA\
  \u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\u4E00\u756A\u306E\
  \u65B9\u6CD5\u306F\u3001`fetch`\u95A2\u6570\u3092\u4F7F\u3046\u3053\u3068\u3067\u3059\
  \u3002\u4EE5\u4E0B\u304C\u7C21\u5358\u306A\u4F8B\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.675363-06:00'
model: gpt-4-1106-preview
summary: "JavaScript\u3067\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\
  \u30ED\u30FC\u30C9\u3059\u308B\u4E00\u756A\u306E\u65B9\u6CD5\u306F\u3001`fetch`\u95A2\
  \u6570\u3092\u4F7F\u3046\u3053\u3068\u3067\u3059\u3002\u4EE5\u4E0B\u304C\u7C21\u5358\
  \u306A\u4F8B\u3067\u3059."
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

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
