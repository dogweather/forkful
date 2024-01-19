---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何と何のため？(What & Why?)

ランダムな数値の生成は、非予測可能な値を生み出すプロセスです。これは、テストデータの生成、ゲームの結果をランダム化するためなど、多数のプログラミング状況で必要とされます。

## どのように行うか (How to)

Clojureでは、`rand`という関数を使いランダムな数値を生成することができます。`rand`関数は0〜1の範囲内でランダムな浮動小数を生成します。

```Clojure
(println (rand))
```

出力結果には、例えば以下のような値が表示されます。


```Clojure
0.7346416871403713
```

特定の範囲のランダムな整数が必要な場合は、`rand-int` 関数を使います。`rand-int n`は 0（含む）からn（含まない）までのランダムな整数を生成します。

```Clojure
(println (rand-int 10))
```

出力は以下のようになります。

```Clojure
6
```

## 深堀り (Deep Dive)

ランダムな数値の生成に関しては、歴史的な文脈や他の代替手段、そして実装詳細など、さらに深く掘り下げることができます。
   
1. **歴史的文脈**：初めてコンピュータでランダムな数値が生成されたのは、1950年代にIBMの研究者により始められました。それ以来、さまざまな手法が提案され、改良されてきました。

2. **代替手段**：Clojureの他にも、Java、Python、Rubyなど、ほとんどのプログラミング言語にはランダム数生成のための関数またはメソッドが存在します。

3. **実装詳細**：Clojureの`rand`と`rand-int`関数はJavaの`java.util.Random`クラスに基づいています。これは疑似ランダムな数値の生成器であり、シード値によって結果が決まります。

## 参照資料 (See Also)

- Clojure公式ドキュメント: [`rand`](https://clojuredocs.org/clojure.core/rand), [`rand-int`](https://clojuredocs.org/clojure.core/rand-int)
- "ランダムな数値生成"についてのWikipediaの記事: [Link](https://en.wikipedia.org/wiki/Random_number_generation)