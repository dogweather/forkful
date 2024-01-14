---
title:    "Clojure: テキストの検索と置換"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

プログラムを通してテキストを検索や置換をすることは、エンジニアにとって非常に重要です。テキストを簡単に変更することができるため、より効率的なコーディングが可能になります。

## 使い方

Clojureでは、```(clojure.string/replace text search replace)```コマンドを使用してテキストの検索と置換を行うことができます。下記の例を参考にしてください。

```Clojure
; テキストの中から"Hello"を探し、"こんにちは"に置換する
(clojure.string/replace "Hello world" "Hello" "こんにちは") 
; => "こんにちは world"

; 大文字と小文字を区別せずに検索する場合は、```replace-first```を使用する
(clojure.string/replace-first "Hello world" #"hello" "こんにちは")
; => "こんにちは world"
```

文字列でなく正規表現を使用することもできます。例えば、テキスト内の全ての数字を置換する場合は、下記のようになります。

```Clojure
(clojure.string/replace "5 apples and 7 oranges" #"\d+" "10")
; => "10 apples and 10 oranges"
```

Clojureでは、テキスト内の複数の置換も同時に行うことができます。例えば、下記のようになります。

```Clojure
(clojure.string/replace "Hello world" {"Hello" "こんにちは" "world" "世界"})
; => "こんにちは 世界"
```

## 詳細を追う

Clojureの ```clojure.string``` ライブラリには、テキスト検索と置換に役立つ多くの便利な関数が用意されています。```replace```, ```replace-first```, ```split```, ```join```などがありますので、是非試してみてください。

さらに、正規表現を使用する際には、Clojureの```re-find```や```re-seq```関数を活用することで、より高度な検索が可能になります。また、Clojureのベクターやマップなどのデータ構造を使用することで、テキストの便利な操作を行うこともできます。是非、ドキュメントやユーザーガイドを参考にして、様々な機能を学んでみてください。

## 参考リンク

- [Clojure公式ドキュメント](https://clojuredocs.org/clojure.string/replace)
- [正規表現チュートリアル](https://www.regular-expressions.info/tutorial.html)
- [Clojureで正規表現を使用する方法](https://clojuredatascience.com/2019/08/28/regular-expression-in-clojure/)