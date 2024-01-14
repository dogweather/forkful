---
title:    "Clojure: 「正規表現の使用」"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使用する理由は何でしょうか？正規表現は、テキストのパターンマッチングや検索を行うための強力なツールです。テキスト処理やデータ抽出を行う際に役立つため、プログラミングの世界では広く使用されています。

## 使い方

正規表現を使用するには、まず「re」ライブラリをClojureプログラムにインポートする必要があります。例えば、文字列からメールアドレスを抽出することを考えてみましょう。

```Clojure
(require '[clojure.string :as str])
(require '[clojure.repl :refer [source]])
(def email "example@email.com")
(str/split email #"@") ; => ["example" "email.com"]
```

上記のコードでは、「re」ライブラリの「split」関数を使用し、メールアドレスを「@」で区切って配列として返しています。また、Clojureの文字列処理ライブラリである「clojure.string」を使用し、文字列からメールアドレスの部分を取得しています。

## ディープダイブ

正規表現には様々な特殊文字が存在します。例えば、「^」は文字列の先頭を表し、「$」は文字列の末尾を表します。これらの特殊文字を組み合わせることで、パターンをより詳細に指定することができます。また、正規表現では量指定子を使用し、特定の文字やパターンの出現回数を指定することができます。

Clojureでは、「#””」という記法を使用することで、正規表現を文字列として直接指定することができます。これにより、パターンの作成がより柔軟になります。

## 関連リンクを見る

- [Clojureで正規表現を使う方法](https://qiita.com/SunaharaKawakami/items/86a9dcb546a817a59b4f)
- [正規表現レファレンス](http://clojure.github.io/spec.alpha/)