---
title:                "「標準エラーに書き込む」"
html_title:           "Clojure: 「標準エラーに書き込む」"
simple_title:         "「標準エラーに書き込む」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## これは何？ 
標準エラーへの書き込みとは、プログラマーがプログラムの実行中に重要なエラーを検出した場合に、それをコンソールに表示することを意味します。プログラマーたちがこの方法を使用する理由は、エラーをすばやく発見し、デバッグするためです。

## 方法：
```Clojure
(defn write-to-stderr [error-msg]
  (io/println (System/err) error-msg))

(write-to-stderr "An error has occurred")
```

このコードを実行すると、エラーメッセージが標準エラーに表示されます。
```
An error has occurred
```

## 深堀り：
標準エラーへの書き込みは、プログラミングの世界で古くから使用されてきたテクニックです。これは、通常の標準出力とは異なり、エラー出力だけに使用されることに注意してください。代替手段としては、ログファイルへの書き込みや、GUIを使用したエラーダイアログの表示があります。標準エラーへの書き込みは、プログラムの実行中に必要な情報を表示するためにも使用されます。

## 参考：
- [JavaのSystem.errを使用したエラーメッセージの表示](https://docs.oracle.com/javase/jp/8/docs/api/java/lang/System.html#err)