---
title:                "Javascript: ウェブページをダウンロードする"
simple_title:         "ウェブページをダウンロードする"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

ウェブページをダウンロードするメリットについて、ご紹介します。ウェブページをダウンロードすることで、オフラインでもそのウェブページを閲覧することができます。また、ウェブページ内の情報を取得することができるため、特定の目的のためにデータを収集することも可能です。

## 方法

まず、ウェブページをダウンロードするためには、Javascriptで使われるfetch APIを使用します。このAPIは、指定したURLからデータを取得し、そのデータを処理するためのメソッドを提供します。

以下は、ウェブページをダウンロードし、その内容をコンソールに表示するサンプルコードです。

```Javascript
fetch('https://example.com')
  .then(response => response.text()) // レスポンスの内容をテキストとして取得
  .then(data => console.log(data)); // コンソールにデータを表示
```

上記のコードを実行すると、指定したURLのウェブページの内容を取得し、コンソールに表示することができます。また、`response`オブジェクトから様々な情報を取得することもできます。

## 深堀り

ウェブページをダウンロードする際、`fetch`メソッド以外にも`XMLHttpRequest`や`jQuery`などの方法もあります。また、取得したデータをJSON形式で取得することも可能です。

これらの方法や詳細な使い方については、[MDNのドキュメント](https://developer.mozilla.org/ja/docs/Web/API/URLSession/fetch)や[公式のドキュメント](https://developer.mozilla.org/ja/docs/Web/API/XMLHttpRequest)を参考にすることができます。

## 参考リンク

- [Fetch APIを使ってみよう | MDN](https://developer.mozilla.org/ja/docs/Web/API/URLSession/fetch)
- [XMLHttpRequest | MDN](https://developer.mozilla.org/ja/docs/Web/API/XMLHttpRequest)
- [jQueryを使ってみよう | jQuery](https://jquery.com/)