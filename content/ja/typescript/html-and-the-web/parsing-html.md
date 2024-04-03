---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:19.179348-07:00
description: "\u65B9\u6CD5\uFF1A \u59CB\u3081\u308B\u306B\u306F\u3001`node-html-parser`\u306E\
  \u3088\u3046\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\
  \u30EB\u3057\u307E\u3059\u3002\u3053\u3061\u3089\u304C\u30BF\u30FC\u30DF\u30CA\u30EB\
  \u30B3\u30DE\u30F3\u30C9\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.754128-06:00'
model: gpt-4-0125-preview
summary: "\u59CB\u3081\u308B\u306B\u306F\u3001`node-html-parser`\u306E\u3088\u3046\
  \u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\
  \u307E\u3059\u3002\u3053\u3061\u3089\u304C\u30BF\u30FC\u30DF\u30CA\u30EB\u30B3\u30DE\
  \u30F3\u30C9\u3067\u3059\uFF1A."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

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
