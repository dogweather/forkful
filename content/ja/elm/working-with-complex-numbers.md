---
title:                "複素数の扱い方"
date:                  2024-01-26T04:40:08.857339-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
複素数は実数と虚数の組み合わせで、`a + bi` のように表されます。ここで `i` は -1 の平方根です。複素数は、通常の数では解けない問題を解決するために、工学や物理学などの分野で重要です。

## 方法：
Elmは複素数をサポートしていませんので、自分の型と関数を作成します。以下は簡単な設定です：

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- 例：
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum は { real = 4.0, imaginary = -2.0 }
```

## 掘り下げ
歴史的に、複素数は常に受け入れられていたわけではありません。16世紀になって、立方方程式を解くためのゲームチェンジャーとなりました。Pythonなど他の言語では、箱から出してすぐに使える複素数サポートと演算を提供しています。ご覧の通り、ElmではDIYのアプローチが必要です。しかし、掛け算、割り算、その他の演算を構築し、パフォーマンスの問題を調整することで、必要なほど洗練されたものにすることができます。

## 参照
- Elmの公式ドキュメント：https://package.elm-lang.org/ カスタムタイプの作成とElmの基本をマスターするために。
- 数学史に興味がある方は、Paul J. Nahin著の「An Imaginary Tale」で、複素数が時間を通じてどのように進化したかをチェックしてみてください。
- 複素数の魔術を活かす数学指向のプログラミングチャレンジには、プロジェクトオイラー（https://projecteuler.net）に挑戦してみてください。