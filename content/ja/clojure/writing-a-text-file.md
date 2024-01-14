---
title:                "Clojure: テキストファイルの作成"
simple_title:         "テキストファイルの作成"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

# なぜ？

テキストファイルを書くメリットは何でしょうか？テキストファイルはシンプルで、様々なプログラミング言語で読み書きが可能です。また、プログラムやデータの保存や共有にも便利です。

# 書き方

```Clojure
;;テキストファイルを作成する
;;ファイル名は「sample.txt」とします
(with-open [file (clojure.java.io/writer "sample.txt")]
  (.write file "こんにちは、世界！"))

;;ファイルの内容を読み込む
(with-open [file (clojure.java.io/reader "sample.txt")]
  (println (.readLine file)))

;;出力
;;こんにちは、世界！
```

# 詳しい解説

テキストファイルを作成する際には、`with-open`マクロを使用してファイルを開き、`clojure.java.io/writer`関数を使用してテキストを書き込みます。同様に、`with-open`マクロと`clojure.java.io/reader`関数を使用してファイルを開き、ファイルの内容を読み込むことができます。

また、テキストファイルの作成には、他にも`spit`関数や`slurp`関数などの便利な関数があります。詳しくは公式ドキュメントをご確認ください。

# 参考リンク

- Clojure 公式ドキュメント: https://clojure.org/
- "テキストファイルの作成方法" (TechAcademy): https://www.tech- academy.jp/magazine/21662
- "Clojure でテキストファイルの読み書き" (Qiita): https://qiita.com/uchan_nos/items/e482124d49042a17555c