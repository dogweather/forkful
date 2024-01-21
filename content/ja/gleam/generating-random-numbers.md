---
title:                "ランダム数の生成"
date:                  2024-01-20T17:49:21.330570-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
ランダム数の生成とは、予測不可能な数を作り出す過程です。プログラマーはこれを利用してシミュレーション、ゲーム、セキュリティなど多岐に渡る用途に応じて使います。

## How to:
Gleamでは簡単にランダム数を生成できます。以下にコード例と出力サンプルを示します。

```gleam
import gleam/erlang
import gleam/bit_builder
import gleam/io

pub fn main() {
  let seed = erlang.system_time() |> bit_builder.from_int32 |> bit_builder.to_bit_string
  let random_num = erlang.rand_uniform(1, 100, seed)
  
  io.debug(random_num)
}
```

実行すると、1から100までの間でランダムな整数が出力されます。

## Deep Dive
ランダム数生成は古くからコンピュータサイエンスにおいて重要な役割を担っています。多くのアルゴリズムと異なるアプローチが存在し、パフォーマンスや予測不可能性のバランスを取ります。Gleamでのランダム数生成はErlang VMを背景に持っており、そのセキュリティと効率を享受できます。代替手段としては、疑似ランダム数生成器(PRNG)やハードウェアによるランダム数生成器などがあります。実装においては、ランダムなビットストリームを生成してから所望の形式の数値に変換するのが一般的です。

## See Also
- Gleamの公式ドキュメント: [https://gleam.run/](https://gleam.run/)
- Erlangのrandモジュール: [http://erlang.org/doc/man/rand.html](http://erlang.org/doc/man/rand.html)
- ランダム数に関するより広範な話題: [https://en.wikipedia.org/wiki/Random_number_generation](https://en.wikipedia.org/wiki/Random_number_generation)