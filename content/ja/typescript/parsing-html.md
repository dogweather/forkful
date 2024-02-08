---
title:                "HTMLの解析"
aliases:
- ja/typescript/parsing-html.md
date:                  2024-02-03T19:13:19.179348-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTMLの解析"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
