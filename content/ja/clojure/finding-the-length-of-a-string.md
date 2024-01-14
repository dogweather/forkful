---
title:                "Clojure: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることにエンゲージする必要性をご存知ですか？文字列の長さを知ることで、テキスト処理やデータ解析などの多くのプログラムにおいて、重要な役割を果たすことができます。Clojure を使えば、これを実現する方法も簡単に学ぶことができます。

## 方法

文字列の長さを求めるには、`count` 関数を使用します。この関数は文字列の長さを返し、引数には文字列を指定します。以下は、SandClojure.org 上でスニペットとして実行された例です。

```Clojure
(count "こんにちは") ; 出力結果は 5
(count "") ; 出力結果は 0
```

Clojure では、文字列はシーケンスとして扱われるため、`count` 関数は他のデータ構造でも使用することができます。例えば、リストの要素数を数えることもできます。

```Clojure
(count [1 2 3]) ; 出力結果は 3
```

## ディープダイブ

Clojure の `count` 関数は、数えられるデータ構造に応じて柔軟に動作します。文字列だけでなく、リストやベクターなどのシーケンス、さらにはマップや集合なども引数として受け取ることができます。また、`count` 関数は、単語や文字ではなく文字数で数えるため、マルチバイト文字も正しくカウントすることができます。

## See Also

- [Clojure 公式サイト](https://clojure.org/)
- [Clojure の count 関数についてのドキュメント](https://clojuredocs.org/clojure.core/count)
- [SandClojure.org](https://www.sandclojure.org/) - オンラインで Clojure を実行できるサンドボックス環境