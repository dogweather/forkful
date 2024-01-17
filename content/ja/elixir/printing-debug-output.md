---
title:                "デバッグ出力のプリント"
html_title:           "Elixir: デバッグ出力のプリント"
simple_title:         "デバッグ出力のプリント"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何？なぜ？

デバッグの出力をプリントすることは、プログラマーがアプリケーションの実行中に特定の情報を表示したいときに使用する方法です。デバッグの出力は、問題のが原因を見つけるために役立ちます。


## 方法：

Elixirでは```:debugger```モジュールを使用して簡単にデバッグ出力をプリントすることができます。以下のコードをコンパイルし、コードを実行します。

```
defmodule Example do
  require Logger
  use :debugger
  
  def get_name(name) do
    Logger.debug("Name is #{name}")
    name
  end
end

Example.get_name("John")
```

上記のコードを実行すると、コンソールに以下のような出力が表示されます。

```
22:02:47.418 [debug] Name is John
```

この出力から、私たちが指定した名前が「John」であったことがわかります。これにより、私たちは特定の部分が機能しないということがわかります。

## 深堀り：

デバッグの出力は、プログラミングにおいて非常に重要なテクニックです。プログラム中にロギングを行うことで、プログラムの実行中に発生した情報を取得することができます。また、デバッグの出力を使用することで、コードをテストする際にも役立ちます。代わりに、デバッグ出力をブレークポイントとして使用することもできます。

Elixir以外の言語でも、同様のデバッグ技術を使用することができます。ただし、それぞれの言語には独自のデバッグツールがあります。

デバッグの出力は、プログラム開発のフェーズを選ばずに使用することができます。デバッグ出力を使用することで、プログラムを追跡し、問題を解決することができます。

## 関連リンク：

- Elixir オフィシャルドキュメント: https://elixir-lang.org/docs.html
- Elixirのデバッグ文書: http://debugger.exhale-db.com/elixir/debugging.html