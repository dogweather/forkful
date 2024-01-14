---
title:    "Elm: 一時ファイルの作成"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ
一時ファイルを作成することに取り組む理由はたくさんあります。たとえば、一時的なデータの保存やプログラムの実行中に必要な一時ファイルを作成するためです。

## 作り方
一時ファイルを作成するには、Elm の `File` モジュールを使用します。まず、`File.System` モジュールから `open` 関数を使用して、一時ファイルを作成します。次に、作成した一時ファイルに対して、データを書き込んだり読み出したりすることができます。例えば、以下のように書くことができます。

```elm
import File.System exposing (..)

main =
  open "temp.txt" Write
    `andThen` \handle ->
        write handle "This is temporary data"
          `andThen` \_ ->
              read handle
                  `andThen` \data ->
                      Html.text data
```

以上のコードでは、`open` 関数を使用して `temp.txt` という名前の一時ファイルを作成し、`write` 関数でデータを書き込み、`read` 関数でデータを読み出しています。

## 深堀り
一時ファイルを作成する際には、いくつかの注意点があります。まず、一時ファイルはプログラムが終了した時点で自動的に削除されるので、パーマネントなデータとして保存することはできません。また、一時ファイルを作成する際には、OS のファイルシステムにアクセスするため、セキュリティの観点から注意が必要です。

## また見る
- [Elm File モジュールのドキュメント](https://package.elm-lang.org/packages/elm/file/latest/)
- [一時ファイルの作り方 (英語)](https://www.geeksforgeeks.org/creating-a-temporary-file-in-python/)