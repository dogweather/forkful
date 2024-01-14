---
title:                "Javascript: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

「なぜ文字列の長さを求めるのか？」

あなたが JavaScript を学んでいるなら、文字列の長さを求めることは非常に便利です。文字列の長さを知ることで、その文字列がいくつの文字から構成されているのかを把握することができます。例えば、ゲームやアプリでプレイヤーの名前を入力する際に、入力された文字列の長さを確認して制限を設けることができます。このように、文字列の長さを求めることはプログラミングにおいて非常に重要な機能の一つです。

## 使い方

```Javascript
let str = "こんにちは！";
console.log(str.length); // 5
```

上記のようなコードを実行すると、変数 `str` に格納された文字列の長さが取得できます。`str.length` のように「.length」を付けることで、文字列の長さを求めることができます。また、日本語の文字数も正しく取得できるので安心して使うことができます。

## 詳細解説

文字列の長さを求めるためには、実際には内部的にはループ処理が行われています。一つずつ文字をカウントしていき、最後にカウントした数が文字列の長さになります。このように、実際にはコンピューターが文字列をどのように処理しているのかを知ることは、プログラミングを学ぶ上で非常に役に立つことです。

「見てわかるJavaScript」 https://booth.pm/ja/items/765495

「JavaScript入門 」 https://www.javascript.com/

「JavaScript講座 - レッスン 2 教科# 8 文字列の長さを取得する」https://www.javadrive.jp/javascript/string/index2.html

## 関連リンク

「文字列の長さを求める方法のまとめ」https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/length

「String.prototype.length」https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/length

「文字列の長さを求める -string.lengthプロパティ」https://www.sejuku.net/blog/43644