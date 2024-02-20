---
date: 2024-01-20 17:45:13.590370-07:00
description: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\
  \u308B\u3068\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u304B\u3089\u60C5\
  \u5831\u3092\u81EA\u5206\u306E\u30C7\u30D0\u30A4\u30B9\u306B\u4FDD\u5B58\u3059\u308B\
  \u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u30C7\u30FC\u30BF\
  \u53CE\u96C6\u3001\u30C6\u30B9\u30C8\u3001\u3042\u308B\u3044\u306F\u30AA\u30D5\u30E9\
  \u30A4\u30F3\u3067\u306E\u5206\u6790\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:00.940819
model: gpt-4-1106-preview
summary: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\
  \u3068\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u304B\u3089\u60C5\u5831\
  \u3092\u81EA\u5206\u306E\u30C7\u30D0\u30A4\u30B9\u306B\u4FDD\u5B58\u3059\u308B\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u30C7\u30FC\u30BF\u53CE\
  \u96C6\u3001\u30C6\u30B9\u30C8\u3001\u3042\u308B\u3044\u306F\u30AA\u30D5\u30E9\u30A4\
  \u30F3\u3067\u306E\u5206\u6790\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
Webページをダウンロードするとは、インターネットから情報を自分のデバイスに保存することです。プログラマはデータ収集、テスト、あるいはオフラインでの分析のためにこれを行います。

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
