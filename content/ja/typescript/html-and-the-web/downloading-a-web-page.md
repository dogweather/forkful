---
date: 2024-01-20 17:45:13.590370-07:00
description: "How to: (\u65B9\u6CD5) TypeScript\u3067Web\u30DA\u30FC\u30B8\u3092\u30C0\
  \u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\u57FA\u672C\u7684\u306A\u65B9\u6CD5\u3092\
  \u898B\u3066\u3044\u304D\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.755722-06:00'
model: gpt-4-1106-preview
summary: "TypeScript\u3067Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3059\u308B\u57FA\u672C\u7684\u306A\u65B9\u6CD5\u3092\u898B\u3066\u3044\u304D\
  \u307E\u3057\u3087\u3046."
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## How to: (方法)
TypeScriptでWebページをダウンロードする基本的な方法を見ていきましょう。

```TypeScript
import axios from 'axios';
import fs from 'fs';

async function downloadWebPage(url: string): Promise<void> {
  try {
    const response = await axios.get(url);
    fs.writeFileSync('page.html', response.data);
    console.log('Download complete.');
  } catch (error) {
    console.error('An error occurred:', error);
  }
}

downloadWebPage('http://example.com');
```

実行すると、`page.html`ファイルに内容が保存されます。

```
Download complete.
```

## Deep Dive (掘り下げ)
Webページをダウンロードする理由はたくさんあります。初期のインターネットでは、ウェブブラウザがまだ一般的でなく、コマンドラインツールを使ってHTMLコンテンツを取得していました。今日では、Node.jsやその外部モジュールの`axios`のようなツールがこの作業を容易にします。代わりに`fetch`APIを使用することもできますが、Node.jsの環境では`node-fetch`モジュールをインストールする必要があります。詳細については、APIドキュメントを参照してください。

## See Also (関連情報)
- [Axios GitHub repository](https://github.com/axios/axios)
- [Node.js `fs` module documentation](https://nodejs.org/api/fs.html)
- [Using `node-fetch` for server-side requests](https://www.npmjs.com/package/node-fetch)
- [MDN Web Docs on the Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
