---
title:    "Elixir: 部分文字列の抽出"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## 私たちがElixirで文字列を抽出する理由

抽出は、文字列を操作するときに非常に有用な方法です。文字列を分解して、必要な部分だけを取り出せるようにすることで、より効率的にプログラミングすることができます。また、文字列からのサブセットの抽出は、様々なデータ処理や文字列解析の手段としても重要です。

## 方法

文字列からサブストリングを抽出する方法は、Elixirで非常にシンプルです。まず、抽出したい部分の開始インデックスと終了インデックスを指定します。そして、文字列の `String.slice/3` 関数を用いて、抽出したい部分を指定します。例えば、次のように書きます。

```elixir
str = "こんにちは、私はElixirを学んでいます"
String.slice(str, 5, 11)
```

この場合、出力は `ちは、私は` となります。

また、正規表現を使用して、より柔軟に抽出することもできます。例えば、次のように書きます。

```elixir
str = "私のメールアドレスはtest@example.comです"
Regex.run(~r/[\w\d\.\-]+@[\w\d\.\-]+\.\w{2,4}/, str) |> hd
```

この場合、出力は `test@example.com` となります。

## 深堀り

サブストリングの抽出は、配列やリストの取り出しと同様に、0から始まるインデックスを使用します。また、終了インデックスは省略することもできます。この場合、文字列の最後までを抽出することになります。

さらに、`String.split/3` 関数を用いて、文字列を指定したデリミタで分割し、抽出することもできます。

また、文字列の結合や変換を行う前に、文字列の抽出を行うことで、より効率的なコーディングを行うことができます。

## その他

これら以外にも、Elixirでは文字列を操作するための多数の関数を提供しています。是非、公式ドキュメントや参考サイトを見て、より詳細な理解を深めてください。

## 関連リンク

- Elixir公式ドキュメント: https://hexdocs.pm/elixir/String.html
- Elixir School: https://elixirschool.com/jp/lessons/basics/basics/#6-string
- Elixirの文字列操作について学ぼう: https://dev.classmethod.jp/etc/elixir-string-functions/