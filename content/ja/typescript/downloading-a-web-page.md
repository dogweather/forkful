---
title:                "ウェブページをダウンロードする"
html_title:           "TypeScript: ウェブページをダウンロードする"
simple_title:         "ウェブページをダウンロードする"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
Webページをダウンロードするとは何かを説明する2〜3文と、プログラマーがそれを行う理由を説明します。

Webページのダウンロードは、インターネット上で情報を取得するプロセスです。プログラマーは、この方法で必要なデータを取得し、処理することができるようになります。

## 方法：
```TypeScript
const https = require('https');
const URL = "https://www.example.com";
https.get(URL, function(response) {
  response.setEncoding("utf-8");
  let body = "";
  response.on("data", data => {
    body += data; //データを取得
  });
  response.on("end", () => {
    console.log(body); //ダウンロードしたWebページのコンテンツを表示
  });
});
```

## 深堀り：
Webページをダウンロードする方法には、他にも様々な手段があります。例えば、ブラウザーの開発者ツールを使って、手動でコードを実行することも可能です。

また、Node.jsやPythonなどのプログラミング言語でもWebページをダウンロードすることができます。それぞれの言語に特有のライブラリを使用することで、より簡単に実装することができます。

## 関連情報：
Webページをダウンロードするときに役立つリンクを紹介します。

- [Node.js公式ドキュメント: https.get()](https://nodejs.org/api/https.html#https_https_get_url_options_callback)
- [Python公式ドキュメント: urllib.request](https://docs.python.org/3/library/urllib.request.html)
- [ブラウザの開発者ツールを使ったWebページのダウンロード方法: https://www.lifewire.com/fully-download-files-in-chrome-4582686](https://www.lifewire.com/fully-download-files-in-chrome-4582686)