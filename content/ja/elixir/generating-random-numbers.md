---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?（何となぜ？）

ランダムな数字を生成するとは、数学的に予測不可能な数列を生み出すことを意味します。これは、ゲームのエレメントのランダム化や暗号化技術など、多くのプログラミングタスクにとって必要な作業です。

## How to:（方法について）

Elixirでランダムな数値を生成する最も簡単な方法は、組み込みの`:rand.uniform/1`関数を使用することです。以下にその例を示します：

```elixir
random_num = :rand.uniform(100)
IO.puts("The random number is #{random_num}")

# This will output: "The random number is 34" (or any other number between 1 and 100)
```

このコードは、1から100までの間のランダムな数字を生成し、それをコンソールに表示します。

## Deep Dive（深く掘り下げて）

ランダム数値生成は古くから存在し、古代ローマ時代にはダイスが使用されていました。また現代では、コンピューターにおけるランダム数の生成は非常に重要な問題となっており、その品質はシミュレーションの正確さやセキュリティの強度に直接影響を与えます。

Elixirの`:rand.uniform/1`関数は実際には完全なランダム性を提供していません。代わりに、疑似ランダムな数値を生成します。これは、開始点になる「種」に基づいて計算される数字の列を意味します。種が同じであれば、生成される数値列も同じになります。

Elixir以外の言語では、さまざまなアプローチでランダム数値生成が実装されています。Pythonでは`random`モジュール、Javaでは`java.util.Random`クラスが用意されています。

## See Also（関連情報）

- Elixirの`:rand.uniform/1`関数についての公式ドキュメンテーション：[https://erlang.org/doc/man/rand.html#uniform-1](https://erlang.org/doc/man/rand.html#uniform-1)
- ランダム数生成に関するWikipediaの記事：[https://en.wikipedia.org/wiki/Random_number_generation](https://en.wikipedia.org/wiki/Random_number_generation)
- PythonとJavaでのランダム数値生成の比較：[https://realpython.com/python-random/](https://realpython.com/python-random/)、 [https://www.baeldung.com/java-generate-random-long-float-integer-double](https://www.baeldung.com/java-generate-random-long-float-integer-double)