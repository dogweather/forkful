---
title:                "Elixir: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ
文字列から部分文字列を抽出する場合の実践的な方法について学ぶことができます。

## 方法
```elixir
defmodule Substring do
  def extract(string, index, length) do
    String.slice(string, index, length)
  end
end
```
### コード例
```elixir
Substring.extract("こんにちは、世界！", 3, 5)
```
### 出力
```
"ちは、"
```
## 深く掘り下げる
部分文字列を抽出するときには、Stringモジュールのslice関数を使用するのが一般的です。また、文字列の位置や長さに応じて、さまざまな方法で部分文字列を抽出することもできます。さらには、文中に存在する複数の部分文字列を一度に抽出する方法もあります。詳しくは公式ドキュメントをご確認ください。

## 関連リンク
- [Elixir 公式ドキュメント](https://hexdocs.pm/elixir/String.html#slice/3)
- [Elixir 公式サイト](https://elixir-lang.org/)
- [Elixir 入門書](https://techscore.github.io/elixir/introduction.html)