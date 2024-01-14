---
title:    "Elixir: 文字列の長さを見つける"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

スピーディーかつ効率的に文字列の長さを知る必要がある場合があります。Elixirのプログラミングを学ぶことで、これを簡単に実現することができます。

## 方法

文字列の長さを知るには、Elixirの組み込み関数である`String.length()`を使います。以下の例を参考にしてください。

```Elixir
string = "こんにちは、世界！"
String.length(string)
```

上記のコードを実行すると、文字列の長さである`9`が出力されます。

## ディープダイブ

Elixirでは、文字列は単なるバイナリではなく、バイナリデータとして扱われます。そのため、文字列の長さを知る際には、バイナリデータとしてどのように処理されるかも重要です。また、文字列の長さを知る際にはUnicodeの扱いにも注意が必要です。

## 参考リンク

- [Elixir公式ドキュメント: String](https://hexdocs.pm/elixir/String.html)
- [Elixir String Module: length/1](https://hexdocs.pm/elixir/String.html#length/1)
- [Understanding Binary Data in Elixir](https://www.poeticoding.com/understanding-binary-data-in-elixir/)
- [Unicode and UTF-8 in Elixir](https://www.poeticoding.com/unicode-and-utf-8-in-elixir/)

## 関連情報を見る