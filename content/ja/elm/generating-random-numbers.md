---
title:                "乱数の生成"
html_title:           "Elm: 乱数の生成"
simple_title:         "乱数の生成"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何か & なぜ?

乱数生成とは、プログラマーがコンピューターやプログラミング言語から無作為に数字を生成することを指します。これにより、プログラムの予測可能性を減らしたり、ゲームや暗号などの特定のアプリケーションで使用されたりします。

## 方法:

偶数を生成するには、 ```Elm Basics``` パッケージ内の ```Random``` モジュールを使用します。例えば、1から10までのランダムな偶数を生成するには、次のように書きます。

```
Elm Random

myRandomNumbers : List Int
myRandomNumbers =
  Random.step 2 1 10

```

出力例:

```
[2, 10, 4, 8, 6]
```

## 深堀り:

乱数生成は、コンピューターサイエンスの発展にとって非常に重要な役割を果たしました。過去には、乱数生成には人工的な方法が使用されていましたが、現在ではコンピューターにより複雑な数式が使用されています。代替手段として、物理的な乱数ジェネレーターや乱数テーブルがあります。 Elm の乱数生成は、Seed という擬似乱数ジェネレーターを基にしており、プログラムの予測性を減らすことができます。

## 関連リンク:

- Elmの公式ドキュメント: http://elm-lang.org/docs/random
- 乱数について詳しく説明した記事: https://medium.com/@halinathegeek/elm-the-random-package-9b8f081fb5e1