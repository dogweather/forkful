---
title:                "ウェブページのダウンロード"
aliases: - /ja/typescript/downloading-a-web-page.md
date:                  2024-01-20T17:45:13.590370-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/downloading-a-web-page.md"
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
