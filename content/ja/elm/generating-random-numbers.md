---
title:    "Elm: ランダムナンバーの生成"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数字を生成することに関わる理由は、プログラミングの様々な分野で役に立つことができるからです。例えば、ゲームやシミュレーション、ランダムなデータを必要とする機能など、さまざまな用途に応用できます。

## 作り方

```Elm
-- ランダムモジュールをインポート
import Random

-- 1から10までのランダムな整数を取得する
Random.int 1 10
```

```Elm
-- ランダムな要素を取得するためのリストを作成
names = ["Suzuki", "Tanaka", "Yamada", "Watanabe"]

-- リストからランダムに要素を取得する
Random.list 1 names
```

```Elm
-- ランダムな浮動小数点数を取得する
Random.float 0 1
```

実行結果例:

```Elm
[8]
["Tanaka"]
0.5268780982713546
```

## ディープダイブ

Elmでは、ランダムな数字を生成するために `Random` モジュールが使われます。このモジュールは、Elmのランダム性を確保するために暗号学的に安全な乱数生成器を用いています。Elmの乱数は再現性があり、同じ入力に対しては同じ結果が出力されるようになっています。

また、Elmでは `Random` モジュール内の関数を組み合わせて、さまざまな方法でランダムなデータを生成することができます。例えば、`int` と `list` を組み合わせることで、指定したリストからランダムな要素を取得することができます。

## 参考リンク

- https://guide.elm-lang.org/effects/random.html
- https://package.elm-lang.org/packages/elm-lang/core/latest/Random
- https://www.elm-tanaka.com/Random