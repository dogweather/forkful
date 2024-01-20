---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ?

ランダムな数値生成は、不特定の値を生成するプロセスです。これはゲーム、セキュリティアプリケーション、統計解析などのプログラムで一般的に使用されます。

## 方法:

JavaScriptでは、`Math.random()`関数を使ってランダムな数を生成することができます。以下に簡単な例を示します。

```Javascript
let random = Math.random();
console.log(random);
```

上のコードを実行すると、0から1までのランダムな小数が表示されます。

```Javascript
let random = Math.floor(Math.random() * 10);
console.log(random);
```

上のコードを実行すると、0から9までのランダムな整数を得ることができます。

## 深潜り:

ランダムな数値生成の概念はコンピューティングの初期から存在し、プログラマーや科学者が統計解析やエンターテイメントアプリケーションを作るために用いられていました。ランダムな数を生成する代替方法としては、セキュリティが重視される場面で使われる暗号学的に安全な `crypto.getRandomValues()`があります。これは、推測や衝突が難しい真にランダムな数値を生成します。しかし、`Math.random()`はJavaScriptの内部エンジンに依存し、各ブラウザにより実装が異なることを考慮に入れてください。

## 参照:

- [Mozilla Developer Network(MDN) – `Math.random()`](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [MDN – `crypto.getRandomValues()`](https://developer.mozilla.org/ja/docs/Web/API/Crypto/getRandomValues)
- [JavaScript Tutorial – Random](http://javascript.info/task/random-min-max)