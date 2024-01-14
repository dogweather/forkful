---
title:                "Elixir: デバッグ出力の印刷"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

デバッグ出力を行う理由を説明します。これは、コード内のエラーを特定し、修正するために非常に重要なツールです。デバッグ出力を使用することで、コードの実行中に発生した値や変数を確認することができ、プログラムの動作を理解するのに役立ちます。

## 方法

Elixir では、`IO.inspect/2`関数を使用してデバッグ出力を行うことができます。以下のようにコード内に記述します。

```Elixir
def foo(x) do
    IO.inspect(x)
    x + 2
end
```

上記の例では、変数 `x` の値を出力し、その後に `x + 2` を返す関数 `foo` を定義しています。コードを実行すると、コンソールに変数 `x` の値が表示されます。

```Elixir
iex> foo(5)
5
7
```

このように、`IO.inspect/2` を使用することで、関数や変数の値を簡単に出力することができます。

## ディープダイブ

デバッグ出力を行う際には、さまざまなオプションを設定することができます。例えば、`IO.inspect/2` の第2引数に `label` を指定することで、出力の前に任意のラベルを表示することができます。また、`IO.inspect/2` の第2引数に `pretty: true` を指定することで、出力を見やすい形式で表示することができます。

更に、`IO.inspect/2` をチェーンすることで、複数のデータを同時に出力することができます。また、`IO.inspect/2` では色付きの出力を行うこともでき、問題の特定に役立ちます。

## See Also

- [Elixir 公式ドキュメント - IO.inspect/2](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Elixir 入門 - デバッグの基礎](https://elixir.jp/learn/basic/debug.html)
- [Deviac - Elixir を使用したデバッグの方法](https://deviac.me/articles/how-to-debug-your-elixir-code/)