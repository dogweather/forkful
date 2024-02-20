---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:19.179348-07:00
description: "HTML\u3092\u89E3\u6790\u3059\u308B\u3068\u306F\u3001HTML\u30B3\u30FC\
  \u30C9\u3092\u7D30\u304B\u304F\u8ABF\u3079\u3066\u60C5\u5831\u3092\u63A2\u3057\u3001\
  \u62BD\u51FA\u307E\u305F\u306F\u64CD\u4F5C\u3059\u308B\u3053\u3068\u3092\u610F\u5473\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\
  \u30BF\u3092\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3057\u305F\u308A\u3001\u30D6\
  \u30E9\u30A6\u30B6\u3092\u81EA\u52D5\u64CD\u4F5C\u3057\u305F\u308A\u3059\u308B\u305F\
  \u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:00.939514
model: gpt-4-0125-preview
summary: "HTML\u3092\u89E3\u6790\u3059\u308B\u3068\u306F\u3001HTML\u30B3\u30FC\u30C9\
  \u3092\u7D30\u304B\u304F\u8ABF\u3079\u3066\u60C5\u5831\u3092\u63A2\u3057\u3001\u62BD\
  \u51FA\u307E\u305F\u306F\u64CD\u4F5C\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\
  \u3092\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3057\u305F\u308A\u3001\u30D6\u30E9\
  \u30A6\u30B6\u3092\u81EA\u52D5\u64CD\u4F5C\u3057\u305F\u308A\u3059\u308B\u305F\u3081\
  \u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "HTML\u306E\u89E3\u6790"
---

{{< edit_this_page >}}

## 何となぜ？

HTMLを解析するとは、HTMLコードを細かく調べて情報を探し、抽出または操作することを意味します。プログラマーは、データをスクレイピングしたり、ブラウザを自動操作したりするためにこれを行います。

## 方法：

始めるには、`node-html-parser`のようなライブラリをインストールします。こちらがターミナルコマンドです：

```bash
npm install node-html-parser
```

それでは、TypeScriptで基本的なHTMLを解析してみましょう：

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Apple</li>
                <li>Banana</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Apple Banana"
```

そして、バナナだけを取得したい場合は：

```typescript
const bananas = root.querySelectorAll('li')[1].textContent;
console.log(bananas);  // "Banana"
```

## 深掘り

HTMLの解析は新しいものではありません。それはウェブの初期の日々からありました。最初は開発者が正規表現を使用していたかもしれませんが、それはすぐに混乱しました。DOMパーサーの出現：安定していますが、ブラウザに縛られています。

`node-html-parser`のようなライブラリは苦痛を抽象化します。これらは、jQueryで行うようにHTMLをクエリすることを可能にしますが、Node.jsでサーバーサイドで行います。それは速く、不適切なHTMLに対して寛容で、DOMに優しいです。

`jsdom`もあります。これは完全なブラウザ環境をシミュレートします。それはより重いですが、より徹底的で、操作と相互作用のための完全なドキュメントオブジェクトモデル（DOM）を作成します。

Cheerioを忘れてはなりません。それは、速度と小さなフットプリントとjQueryのような構文を融合させ、二つの間に幸せに位置しています。

## 参照

もっと知りたい場合は、これらに飛び込んでみてください：
- [DOM Parsing and Serialization W3C Specification](https://www.w3.org/TR/DOM-Parsing/)
- [node-html-parser on GitHub](https://github.com/taoqf/node-html-parser)
- [jsdom GitHubリポジトリ](https://github.com/jsdom/jsdom)
- [Cheerioウェブサイト](https://cheerio.js.org/)
