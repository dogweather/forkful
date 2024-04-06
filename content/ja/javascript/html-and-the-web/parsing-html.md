---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:00:57.304411-07:00
description: "\u65B9\u6CD5\uFF1A JavaScript\u306E`DOMParser` API\u3092\u4F7F\u7528\
  \u3057\u3066HTML\u3092\u89E3\u6790\u3057\u307E\u3057\u3087\u3046\u3002"
lastmod: '2024-04-05T22:38:42.160908-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A JavaScript\u306E`DOMParser` API\u3092\u4F7F\u7528\u3057\
  \u3066HTML\u3092\u89E3\u6790\u3057\u307E\u3057\u3087\u3046\u3002"
title: "HTML\u306E\u30D1\u30FC\u30B9"
weight: 43
---

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
