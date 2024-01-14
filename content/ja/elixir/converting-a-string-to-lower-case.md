---
title:                "Elixir: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Elixirプログラミングをする理由 - なぜ文字列を小文字に変換するか

Elixirは強力なプログラミング言語であり、多くの組み込み関数があります。中でも、文字列を扱うための機能が充実しています。文字列を小文字に変換することは、文章の整形や比較検索などに役立ちます。例えば、ユーザーが入力した文字列を全て小文字に統一することで、データの整合性を保つことができます。このように、Elixirで文字列を小文字に変換することは、より効率的なプログラミングを実現するために役立つのです。

Elixirで文字列を小文字に変換する方法

Elixirでは、文字列を小文字に変換するための組み込み関数であるString.downcase/1を使用します。下記のコードを参考にしてください。

```Elixir
text = "Hello, World!"

String.downcase(text)
```

出力結果：

"hello, world!"


このように簡単に文字列を小文字に変換することができます。また、String.downcase/1は文字列以外のデータ型も受け入れることができ、その際には自動的に文字列に変換してくれます。例えば、数値を渡した場合は文字列に変換して小文字に変換することができます。

さらに、String.downcase/1にはオプションの引数があり、文字列内に含まれる特定の言語や文字セットに対応した変換も行うことができます。詳細はElixirの公式ドキュメントを参照してください。

文字列を小文字に変換する深層

文字列を小文字に変換する際には、Unicodeという仕組みが重要な役割を果たしています。Unicodeは、国際的に使われるさまざまな文字セットを統一的に扱うための規格です。Elixirでは、Unicodeに準拠しているため、文字列を小文字に変換する際にも正確な変換が行われるのです。

See Also
参考リンク：

- Elixir公式ドキュメント：
https://hexdocs.pm/elixir/String.html#downcase/1

- Unicodeの詳細について：
https://unicode.org/charts/