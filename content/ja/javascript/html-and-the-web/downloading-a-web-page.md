---
title:                "ウェブページのダウンロード"
aliases:
- /ja/javascript/downloading-a-web-page.md
date:                  2024-01-20T17:44:50.418874-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

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
