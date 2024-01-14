---
title:                "Elixir: コンピューター·プログラミングの記事：ランダムな数値の生成"
programming_language: "Elixir"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数値を生成することのメリットは何でしょうか？Elixirプログラミングをする際、ランダムな数値を使うことで様々な処理をシミュレーションしたり、テストを行うことができます。また、ランダムな数値を使うことで、データの偏りを防ぐことができ、より正確な結果を得ることができます。

## 使い方

まずはランダムな数値を生成するために必要なElixirのライブラリをインポートします。

```Elixir
import :random
```

次に、任意の範囲内でランダムな整数を生成するために、`uniform/1`関数を使用します。この関数の引数として、ランダムな数値の範囲を指定できます。

```Elixir
random.uniform(1..10)
```

上記の例では、1から10までの範囲内でランダムな整数を生成します。

もし小数点付きのランダムな数値を生成したい場合は、`uniform/1`関数を使用した後、その結果に小数点を追加することで実現できます。例えば、0から1の範囲内でランダムな小数点を生成する場合は以下のようになります。

```Elixir
random.uniform(0..1) + :random.uniform()
```

## 詳細を掘り下げる

Elixirの`random`モジュールには、さまざまな関数が用意されています。例えば、`seed/0`関数を使用することで、生成するランダムな数値の種を指定することができます。また、`srand/1`関数を使用することで、生成されるランダムな数値のシードを変更することもできます。

さらに、Elixirにはシード値を指定することで、再現性を持たせることができる`random.seed/1`関数も用意されています。これにより、同じシード値を指定することで、同じランダムな数値が生成されるため、テストやデバッグに役立ちます。

## 他の記事を参考にする

もしElixirでランダムな数値を生成する方法についてもっと詳しく知りたい場合は、次のリンクを参考にしてください。

- [Elixir 公式ドキュメント - `random`モジュール](https://hexdocs.pm/elixir/Random.html)
- [Qiita - [Elixir] `mix test`で作成する完全なランダムテストの方法](https://qiita.com/cometkim/items/ef73e9562dfc9243a339)
- [Elixir School - Random](https://elixirschool.com/ja/lessons/basics/random/)

## 関連リンク

- [Elixir公式ドキュメント - `random`モジュール](https://hexdocs.pm/elixir/Random.html)
- [Qiita - [Elixir] `mix test`で作成する完全なランダムテストの方法](https://qiita.com/cometkim/items/ef73e9562dfc9243a339)
- [Elixir School - Random](https://elixirschool.com/ja/lessons/basics/random/)