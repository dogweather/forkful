---
title:                "複素数の扱い方"
date:                  2024-01-26T04:39:32.350436-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
複素数には実部と虚部（`3 + 4i`のように）があります。それらは、工学、物理学、および特定の計算問題で使用されます。プログラマーは、シミュレーション、信号処理、および特定の種類の数学問題を効率的に解決するためにそれらを扱います。

## どのようにして：
Elixirには組み込みの複素数がないため、自分で作成するか、`ComplexNum`のようなライブラリを使用します。ここでは、ライブラリを使用した簡単な例を紹介します：

```elixir
# ComplexNumがインストールされていると仮定
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# 複素数を作成してそれらを加算する
c1 = {3, 4}   # 3 + 4iを表す
c2 = {2, -3}  # 2 - 3iを表す
result = ComplexMath.add(c1, c2)
IO.puts "結果は：#{inspect(result)}"
```

この出力になります：
```
結果は：{5, 1}
```

これは、`3 + 4i`と`2 - 3i`の合計が`5 + 1i`であることを意味します。

## 深掘り
複素数は、マイナスの平方根を通常の数字では扱えなかったため、歴史の中で現れました。それらが真剣に受け止められたのは17世紀までなく、レネ・デカルトやジェロラモ・カルダノのような数学者のおかげでした。

Elixirでは、複素数には`{3, 4}`のようなタプルをよく使いますが、車輪の再発明を避けるために専用のライブを使用します。通常、ライブラリの方が優れています。これは、虚数単位'i'（FYI：`i`の二乗は`-1`に等しい）のためにトリッキーになる乗算や除算など、細かい点を処理します。

## 関連情報
これらのリソースもチェックしてみてください：
- ElixirのパッケージマネージャHex用の[ComplexNumライブラリ](https://hex.pm/packages/complex_num)。
- 高度なElixirのトピックや演習については、[Elixir School](https://elixirschool.com/en/)。
- Elixirが内部で使用するErlangの[mathモジュール](http://erlang.org/doc/man/math.html)、その他の数学的なニーズについて。