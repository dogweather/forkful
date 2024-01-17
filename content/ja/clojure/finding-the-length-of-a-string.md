---
title:                "文字列の長さを見つける"
html_title:           "Clojure: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# これは何？: 
文字列の長さを見つけるとは、文字列内に含まれる文字の個数を計算することです。プログラマーが行う理由は、ある文字列の文字数や特定の文字を含むかどうかを判断するためです。

## 方法:
```Clojure
; 文字列の長さを見つける方法
(count "Hello World") ; 11を出力
(count "") ; 0を出力
(count "こんにちは") ; 5を出力 
```

## 深く掘り下げる:
文字列の長さを見つけるために最も単純な方法は、文字列の中の文字数を数えることです。しかし、Clojureでは```count```関数を使うことで、より簡単に文字列の長さを見つけることができます。また、空の文字列は0としてカウントされることに注意してください。

他にも、JavaやC++などの言語では、文字列の長さを表すために特殊な変数（例：```strlen```）を使いますが、Clojureではこのような変数はありません。

## 関連情報:
- [Clojureドキュメント - count](https://clojuredocs.org/clojure.core/count)
- [文字列の長さ - Wikipedia](https://ja.wikipedia.org/wiki/文字列の長さ)