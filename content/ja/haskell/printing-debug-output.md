---
title:                "デバッグ出力の印刷"
html_title:           "Haskell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜプログラマーがデバッグ用の出力を表示するのか

デバッグ用の出力を表示することで、プログラムの実行中に起きていることを確認しやすくなります。特に複雑なプログラムや不具合が起きた際に、どの部分が原因かを特定するのに役立ちます。

## 方法

デバッグ用の出力を表示するための基本的な方法は、`putStrLn`関数を使うことです。以下の例を参考にしてください。

```Haskell
main :: IO ()
main = do
    putStrLn "Start of program" -- プログラムの開始を表示
    -- プログラムの実行中に変数の値を表示
    let number = 5
    putStrLn $ "Number is " ++ show number
```
実行結果は以下のようになります。
```
Start of program
Number is 5
```
変数の値を表示する際には、`show`関数を使う必要があります。

より複雑なデバッグをする場合には、`Debug.Trace`モジュールを使うことができます。以下の例を参考にしてください。

```Haskell
import Debug.Trace (trace)

myFunction :: Int -> Int
myFunction x = trace ("myFunction called with " ++ show x) (x + 1)

main :: IO ()
main = do
    let result = myFunction 5
    putStrLn $ "Result is " ++ show result
```

実行結果は以下のようになります。
```
myFunction called with 5
Result is 6
```
`trace`関数は第一引数の文字列を表示し、第二引数の値をそのまま返します。これで、関数のどの部分が呼び出されたかを確認することができます。

## 深堀り

デバッグ用の出力を表示する際には、`Debug.Trace`モジュールを使うことをおすすめします。このモジュールには、`trace`関数の他にも`traceShow`や`traceId`などの便利な関数が用意されています。さらに、Haskellでは`Debug.Trace`の他にもデバッグ用のライブラリがたくさんありますので、用途に合わせて選んでください。


## 関連リンク

- [Haskellでのデバッグ用の出力](https://www.stackage.org/package/base/docs/System-IO.html#v:putStrLn)
- [デバッグ用のライブラリ一覧](https://github.com/mrkkrp/awesome-haskell-debugging)