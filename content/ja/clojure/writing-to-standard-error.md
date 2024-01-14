---
title:    "Clojure: 「標準エラーへの書き込み」"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

なぜ私たちは標準エラーへの書き込みについて語るのでしょうか？標準エラーは、プログラミングにおいて非常に重要な役割を果たしています。この記事では、Clojureで標準エラーに書き込む方法をご紹介します。

## 使い方

```Clojure
(defn write-to-stderr [message]
  (.write System/err message))
(write-to-stderr "エラーが発生しました。")
```

上記のコードを実行すると、コンソールに"エラーが発生しました。"というメッセージが表示されます。標準エラーを使用することで、エラー処理をより効果的に行うことができます。

## 詳細について

Java Virtual Machine (JVM) では、標準エラーは標準出力とは別のストリームとして扱われます。つまり、標準エラーに書き込まれたメッセージは、通常の標準出力とは別の場所に表示されます。これにより、プログラムの実行中にエラーメッセージを表示することができ、デバッグを行いやすくなります。

標準エラーに書き込む方法は、簡単です。上記のコードのように、System/errオブジェクトのwriteメソッドを使用することで、任意のメッセージを標準エラーに書き込むことができます。

## See Also

- [Clojure 公式ドキュメント](https://clojure.org/index)
- [Java Platform, Standard Edition (Java SE) Documentation](https://www.oracle.com/technetwork/java/javase/documentation/index.html)
- [67 Tips for Java](https://blog.jooq.org/2019/02/16/67-tips-for-java/
)