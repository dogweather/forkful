---
title:    "Elixir: ランダムな数値を生成する"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## なぜ

ランダムな数字を生成することによって、自分の運を試したり新しいゲームを作成したりすることができます。

## 作り方

```Elixir
# 1から10までのランダムな整数を生成する

rand(1..10)
# => 5

# -100から100までのランダムな浮動小数点数を生成する

rand(-100..100)
# => 23.467

# ランダムな文字列を生成する

List.to_string(
  Enum.map(1..10, fn _ -> :rand.uniform(97..122) end)
)  
# => "naofnsecbo"

```

## 詳細を掘り下げる

ランダムな数字を生成する方法は多様であり、どの方法を使うかは目的やニーズによって異なります。例えば、安全性が重要な場合はCryptoモジュールを使用し、高速性が重要な場合は乱数ジェネレーターを使用することができます。

## 参考リンク

- [Elixir 公式ドキュメント: ランダムモジュール](https://hexdocs.pm/elixir/Random.html)
- [Elixir 公式ドキュメント: Cryptoモジュール](https://hexdocs.pm/elixir/Crypto.html)
- [Elixir 公式ドキュメント: Genモジュール（乱数ジェネレーター）](https://hexdocs.pm/elixir/Gen.html)
- [10分で理解するElixirの乱数生成](https://qiita.com/atabaryu/items/79a2b2efe7cb5d2b90c1)