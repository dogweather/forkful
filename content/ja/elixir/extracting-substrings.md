---
title:                "文字列の抽出"
html_title:           "Elixir: 文字列の抽出"
simple_title:         "文字列の抽出"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

# 何をするのか、なぜするのか？
文字列の一部を抽出することは、プログラマーがよく行う作業です。例えば、特定の文字列を含むかどうかを確認したり、特定の位置から指定した文字数だけを抽出したりすることができます。

# やり方：
以下の```Elixir ... ```コードブロックを使って、実際にコードを書いてみましょう。

```
# 文字列から「elixir」を抽出
"Hello Elixir" =~ ~r/elixir/
=> "elixir"

# 特定の位置から指定した文字数を抽出
String.slice("Hello Elixir", 6..11)
=> "Elixir"
```

# 深堀り：
文字列を抽出するという作業は、プログラミングにおいてとても重要です。古くから使われている方法としては、正規表現を使う方法があります。また、Elixirでは`String.slice`関数以外にも、`String.first`や`String.last`などの便利な関数が用意されています。

# 関連情報：
Elixirの文字列操作について詳しく知りたい方は、公式ドキュメントを参考にしてみてください。また、正規表現について知りたい方は、Regexモジュールのドキュメントも参考になります。