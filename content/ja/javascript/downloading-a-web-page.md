---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何となぜ？

ウェブページをダウンロードするとは、ウェブサーバーからインターネットを通じてローカルコンピュータへHTMLなどのデータを転送することです。プログラマーがこれを行う理由は、ウェブページの内容を解析、使い勝手の改善、またはデータ集録を行うためです。

## 実践方法：

以下に示す Javascript コードスニペットは、「node-fetch」を利用してウェブページをダウンロードする方法です。

```Javascript
const fetch = require('node-fetch');

fetch('https://example.com')
    .then(response => response.text())
    .then(data => {
        console.log(data);
    })
    .catch(error => {
        console.error('Error:', error);
    });
```

このスクリプトを実行すると、https://example.com の HTMLをコンソールに出力します。

## 詳解：

ウェブページをダウンロードするというテーマは、インターネットが公開されたときから存在します。最初生のHTMLは非常に基本的でしたが、技術の進歩とともにその複雑性は増してきました。

現在でも Curl や Wget などのコマンドラインツールを使って手動でウェブページをダウンロードすることができますが、 Javascript のようなプログラミング言語を利用の方が自動化やカスタマイズが可能になります。

上記の実例では 'node-fetch' ライブラリを利用しましたが、これだけでなく 'axios' や 'request'-（現在非推奨）などのライブラリもウェブページをダウンロードするために良く使われます。

## 参考リンク：

1. NPM にある 'node-fetch' : 
[https://www.npmjs.com/package/node-fetch](https://www.npmjs.com/package/node-fetch)

2. NPM にある 'axios' :
[https://www.npmjs.com/package/axios](https://www.npmjs.com/package/axios)

3. Mozilla MDN Web Docs の "Fetch API" :
[https://developer.mozilla.org/ja/docs/Web/API/Fetch_API](https://developer.mozilla.org/ja/docs/Web/API/Fetch_API)

4. W3Schools にある "Introduction to AJAX" :
[https://www.w3schools.com/js/js_ajax_intro.asp](https://www.w3schools.com/js/js_ajax_intro.asp)