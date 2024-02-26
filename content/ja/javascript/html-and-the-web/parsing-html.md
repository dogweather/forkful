---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:00:57.304411-07:00
description: "HTML\u306E\u89E3\u6790\u3068\u306F\u3001HTML\u30C9\u30AD\u30E5\u30E1\
  \u30F3\u30C8\u304B\u3089\u30C7\u30FC\u30BF\u3092\u62BD\u51FA\u3059\u308B\u3053\u3068\
  \u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u30A6\u30A7\u30D6\u30B3\u30F3\u30C6\u30F3\u30C4\u3068\u306E\u5BFE\u8A71\u3084\
  \u64CD\u4F5C\u3001\u30C7\u30FC\u30BF\u62BD\u51FA\u306E\u81EA\u52D5\u5316\u3001\u307E\
  \u305F\u306F\u30A6\u30A7\u30D6\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u76EE\u7684\
  \u3067\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.619121-07:00'
model: gpt-4-0125-preview
summary: "HTML\u306E\u89E3\u6790\u3068\u306F\u3001HTML\u30C9\u30AD\u30E5\u30E1\u30F3\
  \u30C8\u304B\u3089\u30C7\u30FC\u30BF\u3092\u62BD\u51FA\u3059\u308B\u3053\u3068\u3092\
  \u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u30A6\u30A7\u30D6\u30B3\u30F3\u30C6\u30F3\u30C4\u3068\u306E\u5BFE\u8A71\u3084\u64CD\
  \u4F5C\u3001\u30C7\u30FC\u30BF\u62BD\u51FA\u306E\u81EA\u52D5\u5316\u3001\u307E\u305F\
  \u306F\u30A6\u30A7\u30D6\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u76EE\u7684\u3067\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "HTML\u306E\u30D1\u30FC\u30B9"
---

{{< edit_this_page >}}

## 何となぜ？
HTMLの解析とは、HTMLドキュメントからデータを抽出することを意味します。プログラマーは、ウェブコンテンツとの対話や操作、データ抽出の自動化、またはウェブスクレイピング目的でこれを行います。

## 方法：
JavaScriptの`DOMParser` APIを使用してHTMLを解析しましょう。

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hello, world!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // 出力：Hello, world!
```

次に、クラスを持つような特定のものを取得しましょう：

```Javascript
const htmlString = `<div><p class="greeting">Hello, again!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // 出力：Hello, again!
```

## 詳細解説
HTMLの解析はウェブが誕生した時からあります。当初はブラウザの仕事でした—ブラウザがHTMLを解析してウェブページを表示しました。時間が経つにつれ、プログラマーはこのプロセスを利用したいと思うようになり、`DOMParser`のようなAPIが生まれました。

代替案は？もちろんあります。`jQuery`やPythonの`BeautifulSoup`のようなライブラリやツールがあります。しかし、JavaScriptのネイティブ`DOMParser`は速くて組み込まれているので、追加のライブラリは必要ありません。

実装面では、`DOMParser`でHTMLを解析すると、`Document`オブジェクトが作られます。これをあなたのHTMLの階層モデルと考えてください。それが手に入れば、通常のウェブページのDOMと同じようにナビゲートや操作が可能になります。

ここでのポイント—解析処理は不正なHTMLに引っかかることがあります。ブラウザは寛容ですが、`DOMParser`はそうではないかもしれません。したがって、複雑なタスクや乱雑なHTMLに対しては、サードパーティのライブラリの方がより良いクリーンアップ作業を行うかもしれません。

## 参照
- `DOMParser` APIについてのMDNウェブドキュメント: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- jQueryの解析機能: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- サーバー用の高速で柔軟かつ軽量なjQueryのコア実装であるCheerio: [Cheerio.js](https://cheerio.js.org/)
- JS非対応の解析について：PythonのBeautifulSoupライブラリ: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
