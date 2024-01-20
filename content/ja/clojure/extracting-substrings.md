---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から部分文字列を抽出するとは、文字列の特定の部分を取得することです。これは、大きなデータ塊から特定の情報を切り出すために、プログラマーがよく行う作業です。

## 方法

Clojureが提供する基本的なsubstring抽出関数を見てみましょう。

```Clojure
(subs "あなたのクロージャ" 2 9)
```

このコードは、"あなたのクロージャ"の中から2番目から8番目までの文字を抽出します。出力結果は次の通りです。

```Clojure
"なたのクロー"
```

## 深掘り

Clojureの `subs` 関数は、Javaの `substring` メソッドから派生しています。Javaでは、`substring`メソッドが易用性と効率性を持つため、それをClojureで利用できるようにしました。

代替手段としては、`clojure.string/split`関数があります。しかし、この方法は正規表現を使って文字列を分割しますので、単純な部分文字列の抽出よりも複雑になります。

`suns`の実装について言えば、Javaの`substring`メソッドと同様に、新たな文字列を作成せずに元の文字列のビューを提供します。しかし、この特性はメモリリークを引き起こす可能性があります。

## 関連リンク

- Clojureの公式ドキュメンテーションの[suns](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/subs) 部分
- [Clojureでの文字列処理の詳細説明](https://www.braveclojure.com/core-functions-in-depth/)