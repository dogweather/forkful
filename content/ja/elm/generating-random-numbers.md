---
title:                "ランダムな数値の生成"
html_title:           "Elm: ランダムな数値の生成"
simple_title:         "ランダムな数値の生成"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

また聞いてくれてありがとう!

## なぜ

乱数を生成することのメリットは、様々なシミュレーションやゲームでのランダムな動作を実現することができることです。プログラマーは、アプリケーションの中で特定の範囲やパターンの乱数を使用することで、より多様な体験をユーザーに提供することができます。

## 作り方

乱数を生成するには、Elmの組み込みモジュールである"Random"を使用します。下記のコード例は、1から10までの整数の中からランダムな値を取得する方法を示しています。

```Elm
import Random exposing (..)

main =
    Random.step (always (Random.int 1 10)) Random.initialSeed
        |> Random.generate RandomNumber

type Msg
    = RandomNumber Int

update msg model =
    case msg of
        RandomNumber number ->
            -- ここでランダムな数値を使用する

```

上記のコードでは、"Random.step"という関数を使って乱数を生成しています。"Random.step"の第一引数には、乱数を生成するアクションを指定します。上記の例では、"Random.int"関数を使用しているため、1から10までの整数を生成するように指定しています。第二引数には、乱数のシードを指定します。ここでは、"Random.initialSeed"を使用して初期の乱数シードを指定しています。

乱数を生成するアクションは、"Random.generate"関数を使用して実行します。この関数は、乱数を生成するたびに"Msg"型のデータを生成します。上記の例では、"RandomNumber"というデータが生成され、"update"関数でこれをハンドリングしてランダムな数値を使用することができます。

## ディープダイブ

Elmでは、"Random"モジュールにはさまざまな乱数を生成する関数が用意されています。以下はその一部の例です。

- Random.float : float型のランダムな数値を生成する
- Random.bool : 真偽値のランダムな値を生成する
- Random.list : リストの中からランダムに要素を取得する
- Random.pair : 2つの要素を持つタプルを生成する
- Random.shuffle : リストの要素をランダムに並び替える

また、"Random"モジュールは、再現性のある乱数を生成することも可能です。例えば、特定のシード値を設定することで、同じ乱数を再現することができます。

```Elm
Random.initialSeed
    |> Random.generate Random.float
        |> Debug.log "First value"

-- 0.11774894851453293


Random.initialSeed
    |> Random.generate Random.float
        |> Debug.log "Second value"

-- 0.5370485445135948


Random.initialSeed
    |> Random.generate Random.float
        |> Debug.log "Third value"

-- 0.8576707990012416


-- 同じシード値を使用することで、同じ乱数を再現できる
Random.initialSeed
    |> Random.generate Random.float
        |> Debug.log "First value"

-- 0.11774894851453293
```

このように、"Random"モジュールを使用することで、さまざまな乱数を生成することができます。

## 関連リンク

- [Elm公式ドキュメント - Randomモジュール](https://guide.elm-lang.org/effects/random.html)
- [Elm in