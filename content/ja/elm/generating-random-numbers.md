---
title:                "Elm: ランダムな数値を生成する"
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数字を生成することの大切さを理解するためには、まずElmのランダムモジュールの基本を知ることが重要です。ランダムな数字を使用することで、ゲームやデータシミュレーションなど、様々なアプリケーションを作成することができます。

## 作り方

まずはランダムモジュールをimportして、次のようなコードを書いてみましょう。

```Elm
import Random exposing (Generator, int)

randomNumber : Int
randomNumber =
    Random.generate int (Random.range 1 10)
```

上記のコードでは、1以上10以下のランダムな整数を生成します。もちろん、必要に応じて最小値や最大値を変更することもできます。

## 深堀り

生成するランダムな数字には、シード値を指定することが重要です。シード値を指定することで、再現性のあるランダム性を保証することができます。また、Elmのランダムモジュールには、整数や浮動小数点数だけでなく、ランダムな文字列やリストを生成するための関数も用意されています。詳細な情報は、公式ドキュメントを参照してください。

## See Also

- Elm ランダムモジュールドキュメント: https://package.elm-lang.org/packages/elm/random/latest/
- Elm 公式ウェブサイト: https://elm-lang.org/