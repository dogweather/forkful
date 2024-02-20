---
date: 2024-01-26 00:50:44.513160-07:00
description: "\u30A8\u30E9\u30FC\u51E6\u7406\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u5185\u3067\u4E88\u671F\u3057\u306A\u3044\u4E8B\u614B\u306B\u5BFE\u51E6\u3059\
  \u308B\u3053\u3068\u3067\u3059\u2014\u30C8\u30E9\u30D6\u30EB\u30E1\u30FC\u30AB\u30FC\
  \u3092\u6271\u3046\u30D0\u30A6\u30F3\u30B5\u30FC\u306E\u3088\u3046\u306A\u3082\u306E\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30B9\u30E0\u30FC\u30BA\u306A\u64CD\
  \u4F5C\u3092\u597D\u307F\u307E\u3059\u3002\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u30C8\
  \u30E9\u30D6\u30EB\u3092\u5236\u5FA1\u3057\u3001\u4E88\u671F\u3057\u306A\u3044\u3053\
  \u3068\u304C\u8D77\u304D\u305F\u6642\u306B\u30B3\u30FC\u30C9\u304C\u3064\u307E\u305A\
  \u304D\u8EE2\u5012\u3059\u308B\u3053\u3068\u3092\u9632\u304E\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:00.842933
model: gpt-4-1106-preview
summary: "\u30A8\u30E9\u30FC\u51E6\u7406\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u5185\u3067\u4E88\u671F\u3057\u306A\u3044\u4E8B\u614B\u306B\u5BFE\u51E6\u3059\
  \u308B\u3053\u3068\u3067\u3059\u2014\u30C8\u30E9\u30D6\u30EB\u30E1\u30FC\u30AB\u30FC\
  \u3092\u6271\u3046\u30D0\u30A6\u30F3\u30B5\u30FC\u306E\u3088\u3046\u306A\u3082\u306E\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30B9\u30E0\u30FC\u30BA\u306A\u64CD\
  \u4F5C\u3092\u597D\u307F\u307E\u3059\u3002\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u30C8\
  \u30E9\u30D6\u30EB\u3092\u5236\u5FA1\u3057\u3001\u4E88\u671F\u3057\u306A\u3044\u3053\
  \u3068\u304C\u8D77\u304D\u305F\u6642\u306B\u30B3\u30FC\u30C9\u304C\u3064\u307E\u305A\
  \u304D\u8EE2\u5012\u3059\u308B\u3053\u3068\u3092\u9632\u304E\u307E\u3059\u3002"
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
---

{{< edit_this_page >}}

## 何となぜ？
エラー処理とは、プログラム内で予期しない事態に対処することです—トラブルメーカーを扱うバウンサーのようなもの。プログラマーはスムーズな操作を好みます。エラー処理はトラブルを制御し、予期しないことが起きた時にコードがつまずき転倒することを防ぎます。

## どのように：
ClojureはそのLispの先祖のように、エラー処理に例外を利用します。物事が悪い方向に進んだときにあなたがどう行動するか、ここで見せます。

例外を投げるのは簡単です：
```Clojure
(throw (Exception. "おっと！何か問題が発生しました。"))
```

例外を捕捉する際に、これを頻繁に行うことになるでしょう：
```Clojure
(try
  ;; リスクのあるコード
  (/ 1 0)
  (catch ArithmeticException e
    (println "ゼロで割ることはできません！"))
  ;; finallyブロックは何があっても実行される
  (finally 
    (println "クリーンアップコードはここに書く。")))
```
上記のcatchブロックのサンプル出力：
```
ゼロで割ることはできません！
クリーンアップコードはここに書く。
```

例外についてより豊かなコンテキストを得るために`ex-info`と`ex-data`を使用する：
```Clojure
(try
  ;; カスタム例外を引き起こす
  (throw (ex-info "カスタムエラー" {:type :custom-failure}))
  (catch Exception e
    ;; カスタム例外からデータを取り出す
    (println (ex-data e))))
```
サンプル出力：
```
{:type :custom-failure}
```

## 深堀り
Clojureにおけるエラー処理の流れは、他のLispやJava（`try-catch`メカニズムを継承している）と根本的に違うわけではありません。それは実用的です。Javaと同様に例外を使用するのが主な方法ですが、`ex-info`と`ex-data`を使用した関数型プログラミング風味の豊かなエラーデータを提供します。

Clojureでのエラー処理の代替方法には、`cats`ライブラリの`either`モナドや、core.asyncのチャネルベースのエラー伝搬など、モナディック構造を使用する方法が含まれます。しかし、これらはより複雑で特定のシナリオで使用されます。

歴史的に、プログラミング言語のエラー処理はシンプルなステータス返却から、現代言語のより洗練された例外処理メカニズムに進化してきました。Clojureはシンプルさと関数型プログラミングのタッチを選択し、古いものと新しいものを組み合わせています。

## 参照
- Clojureの例外処理ガイド: https://clojure.org/guides/exceptions
- より関数型アプローチのための「Cats」ライブラリ: https://github.com/funcool/cats
- 非同期プログラミング用の「Core.async」: https://github.com/clojure/core.async
