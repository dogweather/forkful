---
title:    "Clojure: 「文字列の長さを求める」"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# なぜ
文字列の長さを求めることに関心を持つ理由について説明します。

## 方法
```Clojure
; 文字列を定義
(def s "こんにちは！")
; 文字列の長さを求める
(count s)
```
このコードを実行すると、**6**という値が返されます。これは、文字列の長さが6であることを示しています。Clojureは文字列を文字のリストとして扱うので、count関数を使用することでリストのサイズを取得することができます。

## 詳細
文字列の長さを求めることは、プログラミングにおいて非常に一般的なタスクの一つです。文字列を操作する際に必要な情報を提供する上で非常に重要な機能です。Clojureでは、count関数を使用することで簡単に文字列の長さを取得することができますが、内部的には文字列をリストとして扱っていることに注意する必要があります。

## 参考リンク
- [Clojureの公式ドキュメント](https://clojure.org/reference/strings)
- [Clojureでの文字列操作について](https://clojure.or.jp/doc/strings.html)
- [文字列操作のTips集](https://qiita.com/katsuyoshi_yz/items/7af5f4b21ff12507e684)
- [Clojureでの数値変換と文字列長の関係](https://qiita.com/ironista444/items/503cebc97f788a30d576)
- [テキスト処理についてのClojureチュートリアル](https://clojure.or.jp/doc/about_text_processing.html)