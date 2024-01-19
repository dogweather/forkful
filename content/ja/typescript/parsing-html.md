---
title:                "HTMLの解析"
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## 何となぜ？ 
HTMLパースとは、HTML文書を解析し、意味を抽出するプロセスです。開発者がこれを行う主な理由は、ウェブページから特定の情報を取得し、そのデータをさまざまな目的（スクレイピング、ウェブテスト、サイトの改善など）に使用可能にするためです。

## やり方：
以下に簡単なTypeScriptによるHTML解析の例を示します。

```TypeScript
import jsdom from 'jsdom';

const { JSDOM } = jsdom;

const dom = new JSDOM('<!DOCTYPE html><html><body>Hello world</body></html>');

console.log(dom.window.document.querySelector("body").textContent); // "Hello world"
```

この短いプログラムはHTML文書をパースし、bodyタグの中身を表示します。

## 深く掘り下げる
HTMLのパースはウェブ開発の中心的な部分で、その歴史はウェブ自体とほぼ同じです。古いテクニックは正規表現を使用してHTMLを解析することでしたが、これは完全に機能するための多くのエッジケースと落とし穴があったため、現在はあまり使われていません。
現代のアプローチでは、DOMベースのパーサー（上記の例で使用したJSDOMなど）やHTMLパーサライブラリ（Beautiful SoupやPuppeteerなど）が一般的です。

## 参考資料:
- [JSDOM](https://github.com/jsdom/jsdom)：Node.jsのJavaScriptのためのDOMの実装
- [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)：HTMLとXMLファイルを解析するためのPythonライブラリ
- [Puppeteer](https://github.com/GoogleChrome/puppeteer)：無頭ChromeまたはChromiumブラウザの高水準API