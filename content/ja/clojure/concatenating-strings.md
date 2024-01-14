---
title:    "Clojure: 文字列の結合"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を連結することによって、より長い文字列を作り出すことができます。これは、プログラミングでテキストを操作する際に非常に便利であり、より複雑な文字列を生成することができます。

## やり方

```Clojure
;; 文字列を連結するには、str関数を使用します
(str "こんにちは、" "皆さん。" "今日はいい天気ですね。") 
;; => "こんにちは、皆さん。今日はいい天気ですね。"

;; 文字列と変数を連結する場合、str関数と文字列補間を組み合わせることができます
(def name "太郎")
(str "こんにちは、私の名前は" name "です。") 
;; => "こんにちは、私の名前は太郎です。"
```

## ディープダイブ

Clojureでは、文字列はイミュータブル（変更不可）なデータ構造であるため、文字列を連結する際には注意が必要です。連結された後の文字列が新しいオブジェクトとして生成されるため、大量の文字列を連結するとメモリ使用量が増加し、パフォーマンスの低下を引き起こす可能性があります。そのため、大量の文字列を連結する場合はStringBuilderを使用することを検討してください。

## 参考リンク

- [Official Clojure Documentation on Strings](https://clojure.org/reference/strings)
- [Clojure Tutorial: Concatenating Strings](https://clojure.org/guides/learn/strings)
- [Mastering Clojure Strings](https://purelyfunctional.tv/guide/clojure-strings/) 

## 関連記事

- [Clojure初心者向け：文字列を操作する方法](https://exampleblog.com/clojure-string-manipulation) 
- [Clojureを使ったテキスト処理の基本](https://exampleblog.com/clojure-text-processing)