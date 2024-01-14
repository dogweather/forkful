---
title:                "Elm: 乱数の生成"
simple_title:         "乱数の生成"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜランダムな数字を生成するのか

ランダムな数字を生成することは、多くのプログラミング言語において重要な機能の一つです。例えば、ランダムな数字を使用してゲームを作ることができますし、コンピューターのシミュレーションを行う際にも役立ちます。Elmでも、ランダムな数字を生成する機能があります。ここではその方法を紹介します。

## 生成方法

ランダムな数字を生成するには、ElmのRandomモジュールを使用します。まず、このモジュールをインポートします。

```Elm
import Random
```

次に、Randomモジュールの関数を使ってランダムな数字を生成します。例えば、0から10までのランダムな整数を生成するには、以下のように記述します。

```Elm
Random.int 0 10
```

このコードを実行すると、0から10までの範囲内にランダムに整数が生成されます。また、浮動小数点数を生成する場合は、次のように記述します。

```Elm
Random.float 0 1
```

この例では、0から1までの間に浮動小数点数が生成されます。他にも、様々なランダムなデータを生成するための関数が用意されていますので、ドキュメントを参考にしてみてください。

## 深堀り

ランダムな数字を生成する際には、パーセントエンコーディングでエスケープする必要があります。これは、Elmのコードがブラウザーで実行される際に、URLエンコードされる可能性があるためです。そのため、生成されたランダムな数字を使用する際には、次のように```Http.encodeUri```関数を使ってエスケープする必要があります。

```Elm
Http.encodeUri (toString randomNum)
```

また、生成された数字を使用する前に、必ず同じ値を使用する必要があります。それ以外の場合、結果が一貫性がなくなる可能性があります。

## 参考

- [Elm Randomモジュールドキュメント (英語)](https://package.elm-lang.org/packages/elm/random/latest)
- [Elmハンドブック (日本語)](https://elm-lang.org/ja/docs)
- [Elmオンラインモデルプレイグラウンド (日本語)](http://elm-lang.org/try)