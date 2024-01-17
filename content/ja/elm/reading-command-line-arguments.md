---
title:                "コマンドライン引数の読み取り"
html_title:           "Elm: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
コマンドライン引数の読み込みとは、プログラマーがプログラムに入力したコマンドの引数を読み取ることを指します。これをする理由は、ユーザーがプログラムに対して特定の情報を提供することで、より効率的なプログラミングを可能にするためです。

## 方法：
```Elm
import Platform.Cmd exposing (args)
import Task

main =
    Task.perform (\arguments -> ... ) args
```
コマンドライン引数を読み込むには、Platform.Cmdモジュールのargs関数を使用します。これにより、プログラムに入力されたコマンドライン引数がリストとして取得できます。

```Elm
Task.perform (\arguments ->
    List.map (\arg -> ... ) arguments
) args
```
この例では、リスト内のコマンドライン引数をそれぞれマップし、その結果を返します。

## 詳細：
- **歴史的背景：** コマンドライン引数の読み込みは、古くから存在しているプログラミングの機能です。コンピュータが登場した頃から、プログラマーたちはコマンドライン引数を使用してプログラムの動作を制御してきました。

- **代替案：** Elmでは、コマンドライン引数を読み取る方法としてPlatform.Cmdモジュールのargs関数以外に、関数型プログラミングの基本機能を使用する方法もあります。しかし、Platform.Cmdモジュールはより効率的で便利な方法です。

- **実装の詳細：** Platform.Cmdモジュールのargs関数は、プログラムが実行された際に渡されたコマンドライン引数をArgs型として返します。このArgs型には、入力されたコマンドライン引数のリストが含まれています。

## 関連リンク：
- [Elmコマンドライン引数のドキュメンテーション](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#args)
- [ElmのPlatform.Cmdモジュールのソースコード](https://github.com/elm/core/blob/e652ec2dc19a6ee39be371894a2790d44a590d85/src/Platform/Cmd.elm)