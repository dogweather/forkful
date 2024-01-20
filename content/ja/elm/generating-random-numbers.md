---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？

ランダムナンバーの生成は、予測不可能な数値を作り出すプロセスです。プログラマーがランダムナンバーを使用する理由は多数存在し、ゲームの結果の変動やテストデータの生成などが含まれます。

## どうするか:

以下に、Elmプログラミングでランダムな数値を生成するための基本的なコードを示します。出力も示しています。

```Elm
import Random

generate : Model -> ( Model, Cmd Msg )
generate model =
  let
    cmd =
      Random.generate NewNumber (Random.int 1 100)
  in
  ( model, cmd )
```

このコードが生成する出力は1から100までのランダムな整数です。`NewNumber`は生成されたランダム数字を受け取るメッセージです。

## ディープダイブ:

かつては、物理的なランダムな現象（例えば、放射性分解や熱ノイズ）を用いてランダムな数値を生成していましたが、現在ではアルゴリズムを用いて擬似ランダムな数値を生成するのが一般的です。Elmでは、擬似ランダム生成アルゴリズムに基づく`Random`モジュールが提供されています。

また、Elmでは`Cmd`を用いて副作用を伴う操作を行います。ランダムな数値の生成も副作用を伴うので、コマンドとして扱われます。

## 参照先:

以下のリンクから、Elmにおけるランダムな数値の生成についてさらに詳しく知ることができます。

- Elmの公式ドキュメンテーション: [Random](https://package.elm-lang.org/packages/elm/core/latest/Random)
- Elmを用いてランダムな数値を生成するためのチュートリアル: [Random numbers in Elm](https://guide.elm-lang.org/effects/random.html)
- ランダムな数値の生成についての深い理解を得る: [Random number generator](https://en.wikipedia.org/wiki/Random_number_generation)