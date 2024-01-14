---
title:                "Elm: コマンドライン引数の読み取り"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜコマンドライン引数を読む必要があるのか

コマンドライン引数を読むことで、プログラムの実行時にさまざまなオプションを指定することができます。これにより、プログラムをより柔軟に制御することができるため、プログラミングの効率性や機能性を向上させることができます。

## どのようにコマンドライン引数を読むか

Elmでは、コマンドライン引数を読み取るための「Cmd」モジュールが用意されています。ここでは、コマンドライン引数を読み取り、出力するサンプルコードをご紹介します。

```Elm
import Cmd exposing (run)
import Platform.CmdLine exposing (args)

main =
    run program

program =
    args
        |> List.map toString
        |> String.join ", "
        |> putStrLn
```

このコードをコンパイルして実行すると、コマンドライン引数がカンマ区切りで出力されます。例えば、プログラムを次のように実行した場合、

```
elm make Main.elm --output=app.js --optimize
```

次のような出力が得られます。

```
--output, app.js, --optimize
```

コマンドライン引数を読むことで、処理の流れや挙動を調整することができるため、プログラミングにおいて非常に重要な機能です。

## 深く掘り下げる

コマンドライン引数には、オプションのほかにも、フラグや引数の種類を指定する方法など、さまざまな機能があります。詳しくは公式ドキュメントをご確認ください。

# はじめてのコマンドライン引数

公式ドキュメント：https://guide.elm-lang.org/flag/

# 一歩進んだコマンドライン引数の実装

関数の型に制約をつける方法：https://elmprogramming.com/custom-types-for-command-line-in-elm.html

# さらに深く掘り下げることのできるコマンドラインツール

elm-spaやelm-spa-exampleを参考に：https://github.com/rtfeldman/elm-spa-example

# 参考リンク

- https://guide.elm-lang.org/flag/
- https://elmprogramming.com/custom-types-for-command-line-in-elm.html
- https://github.com/rtfeldman/elm-spa-example