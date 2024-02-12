---
title:                "文字列を大文字にする"
aliases: - /ja/javascript/capitalizing-a-string.md
date:                  2024-02-03T19:05:44.228994-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を大文字にする"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となく理由？
文字列を大文字化するとは、文字列の最初の文字を大文字に変換し、残りの文字はそのままにすることを意味します。この操作は、ユーザー入力のフォーマット設定、名前やタイトルの表示、ユーザーインターフェイステキストの一貫性を保証するために、JavaScriptで一般的に行われます。

## 方法：
JavaScriptには、文字列を直接大文字化する組み込みの方法はありませんが、基本的な文字列操作方法を使用して簡単に実装できます。

### 標準的なJavaScriptを使用
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // 出力: "Hello world"
```

### ES6バージョン
ES6のテンプレートリテラルを使用すると、より簡潔な方法で関数を書くことができます：
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // 出力: "Hello ES6"
```

### Lodashを使用
Lodashは、JavaScriptの値、文字列を含む、を操作・作業するための広範な関数を提供する人気のサードパーティのユーティリティライブラリです。Lodashを使用して文字列を大文字化するには：
```javascript
// まだの方は、まずlodashをインストールしてください: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // 出力: "Lodash example"
```
_Lodashは最初の文字を大文字にするだけでなく、文字列の残りを小文字に変換することに注意してください。これはプレーンなJavaScriptの実装とは少し異なります。_

### CSSを使用（表示目的のみ）
目的がUIでテキストを大文字化することであれば、CSSを使用できます：
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- "Hello css"として表示されます -->
```
**注:** この方法は、JavaScript自体の文字列を変更するのではなく、ウェブページ上でテキストがどのように見えるかを変更します。
