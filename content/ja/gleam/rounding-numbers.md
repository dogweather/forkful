---
title:                "数値の丸め処理"
date:                  2024-01-26T03:45:31.196212-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
数値を丸めるとは、値を指定された最近接の位まで調整することです—2.56を整数まで丸めると3になります。プログラマーはこれを簡略化するため、または特定の数値仕様を満たすために行うことが一般的です。通常は、浮動小数点の精度誤差による微妙な問題を避けるためや、出力をユーザーフレンドリーにするためです。

## 方法：
Gleamでは、私の最後のチェック時点で標準ライブラリに丸め機能は含まれていませんが、Erlangの関数を直接使用して浮動小数点を最も近い整数に丸める方法は次の通りです：

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // 出力: 3
}
```

出力:
```
3
```

異なる精度を考えていますか？たとえば、小数点第二位まで丸める場合。少し数学が必要です：

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // 出力: 2.57
}
```

出力:
```
2.57
```

## 深掘り
歴史的に、数値の丸めは、特に金融や科学の計算では正確さと基準が非常に重視されるので、極めて重要でした。丸めなしでは、どこでも長い小数が出現し、計算を非実用的でエラーが発生しやすくなります。

プログラミングの世界では、組み込み関数から包括的な数学ライブラリまで、さまざまな言語が異なるアプローチを提供します。丸めには異なるルールが関わることがあります。たとえば、「半数を上に丸める」（通常の方法）や「半数を偶数に丸める」（バイアスを避けるために金融計算でよく使われる）などです。

Gleamは、Erlangのルーツを持つ若い言語で、Erlangの堅牢な数値関数セットに依存しています。言語が成長するにつれ、外部ルーチンを呼び出す必要を減らし、ネイティブ関数が導入されるかもしれません。

## 参照
- 数値計算のためのErlangの:mathモジュール：https://erlang.org/doc/man/math.html
- 丸めが厄介になりうる背景について：IEEE浮動小数点標準：https://ieeexplore.ieee.org/document/8766229
- これについての数学に興味がある場合、こちらをチェック "コンピューターサイエンティストが浮動小数点算術について知るべきこと"：https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
