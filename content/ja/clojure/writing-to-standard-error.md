---
title:                "Clojure: 「標準エラーへの書き込み」"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

なぜ標準エラー出力を書くのかと言うと、プログラミングにおいてエラーは避けられないものです。そのため、エラーが発生した時にどのような問題があったのかを把握し、プログラムを改善することができるように、標準エラー出力にメッセージを書くことが重要です。

## 作り方

標準エラー出力にメッセージを書くには、`System/err`を使用します。以下はClojureで標準エラー出力にメッセージを書く例です。

```Clojure
(System/err "エラーメッセージ")
```

出力は次のようになります。

```
エラーメッセージ
```

また、複数の引数を渡すこともできます。

```Clojure
(System/err "このエラーは" "重大です")
```

出力は次のようになります。

```
このエラーは重大です
```

## 深堀り

標準エラー出力にメッセージを書くことで、エラーの発生場所や内容を把握することができます。より詳細な情報を得るために、`e`を使用してエラーのスタックトレースを表示することもできます。

```Clojure
(defn divide [a b]
  (System/err "エラーが発生しました")
  (if (= b 0)
    (/ a b)
    (System/err "エラーは発生しませんでした")))
```

上記の例では、引数`b`が0の場合にエラーが発生するようにしています。実際に実行してみると、以下のような出力が得られます。

```
エラーが発生しました
java.lang.ArithmeticException: Divide by zero
	at clojure.lang.Numbers.divide(Numbers.java:188)
	at user/divide.invokeStatic(user.clj:3)
	at user/divide.invoke(user.clj:1)
	at user/eval63.invokeStatic(user.clj:7)
	at user/eval63.invoke(user.clj:7)
	at clojure.lang.Compiler.eval(Compiler.java:7058)
	at clojure.lang.Compiler.eval(Compiler.java:7048)
	at clojure.core/eval.invokeStatic(core.clj:3214)
	at clojure.core/eval.invoke(core.clj:3210)
	at clojure.repl/user$eval2067$fn__2068.invoke(repl.clj:654)
	at clojure.repl/user$eval2067.invokeStatic(repl.clj:654)
	at clojure.repl/user$eval2067.invoke(repl.clj:654)
	at clojure.repl/repl.invokeStatic(repl.clj:702)
	at clojure.repl/repl.invoke(repl.clj:681)
	at clojure.repl/repl$run.invokeStatic(repl.clj:774)
	at clojure.repl/repl$run.invoke(repl.clj:742)
	at clojure.main/mainOpt.invokeStatic(main.clj:315)
	at clojure.main/mainOpt.invoke(main.clj:311)
	at clojure.main/main.invokeStatic(main.clj:424)
	at clojure.main/main.doInvoke(main.clj:387)
	at clojure.lang.RestFn.invoke(RestFn.java:408)
	at clojure.lang.Var.invoke(Var.java:379)
	at clojure.lang.AFn.applyToHelper(AFn.java:154)
	at clojure.lang.Var.applyTo(Var.java:700)
	at clojure.main.main(main.java:37)
```

このように、エラーが発生した場所から呼び出された関数や、エラーの内容を詳しく確認することができます。

## 参考リンク

- [Clojure公式ドキュメント](https://clojure.org/)
- [標準エラー出力についてのClojure