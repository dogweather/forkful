---
title:                "Elixir: 「文字列の長さを見つける」"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# なぜ

文字列の長さを求めることに関心を持つ理由はたくさんあります。例えば、文字列の長さを求めることで、テキストエディタやデータベースなどのプログラムを改善することができます。また、文字列の長さを知ることで、プログラムの効率を上げることができるでしょう。

# 方法

Elixirで文字列の長さを求める方法は簡単です。まず、文字列を変数に代入します。次に、String.length関数を使って文字列の長さを取得します。

```Elixir
str = "こんにちは、世界！"
String.length(str) # 出力結果：9
```

また、文字列の中に含まれる特定の文字や、複数の単語の長さを求めることもできます。

```Elixir
str = "The quick brown fox jumps over the lazy dog."
String.length("u") # 出力結果：1
String.length("quick fox") # 出力結果：9
```

# 深堀り

文字列の長さを求める際に、多くの開発者が間違えることがあります。それは、マルチバイト文字を含む文字列を扱う場合です。マルチバイト文字を含む文字列は、通常の文字列よりも長さが1文字あたり2〜4倍になるため、注意が必要です。

例えば、"こんにちは、世界！"という文字列を扱う場合、実際の文字数は9ではなく、18になります。これは、日本語の文字はマルチバイト文字であるためです。しかし、ElixirではString.codepoints_length関数を使うことで、正しい長さを取得することができます。

```Elixir
str = "こんにちは、世界！"
String.codepoints_length(str) # 出力結果：9
```

# 他に参考になるリンク

- [Elixir公式ドキュメント - 文字列の操作](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- [Qiita - String.lengthとString.codepoints_lengthの違い](https://qiita.com/kmizu/items/c0b967f21a93f96e2881)