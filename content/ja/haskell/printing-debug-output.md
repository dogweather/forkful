---
title:                "Haskell: デバッグ出力のプリント"
simple_title:         "デバッグ出力のプリント"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

# なぜデバッグ出力をプリントするのか

コンピュータプログラミングをする際、バグやエラーが発生することはよくあります。デバッグ出力をプリントすることは、それらのバグやエラーを特定し、修正するのに役立ちます。

## プリントデバッグ出力の方法

デバッグ出力をプリントするには、Haskellで提供される`putStrLn`関数を使用します。以下の例をご覧ください。

```Haskell
main = do
  putStrLn "デバッグ出力の例"
  x <- readLn :: IO Int
  putStrLn ("入力された数値は " ++ show x ++ " です。")
```

この例では、`putStrLn`関数を使用して、"デバッグ出力の例"というテキストを出力しています。そして、`readLn`関数を使用してユーザーからの入力を受け取り、その数値を`show`関数を使用して文字列に変換し、`putStrLn`関数を使用して表示しています。

実行すると、以下のような結果が得られます。

```
デバッグ出力の例
10
入力された数値は 10 です。
```

## プリントデバッグ出力の深堀り

デバッグ出力をプリントすることは非常に重要ですが、過度に使用するとプログラムのパフォーマンスに影響を与える可能性があります。そのため、デバッグ出力はバグやエラーの特定のためにのみ使用し、本番環境では削除することをお勧めします。

また、Haskellでは`Debug.Trace`モジュールを使用することで、より詳細なデバッグ出力を提供することができます。詳細な情報を必要とする場合は、このモジュールを使用してください。

## 参考リンク

- [HaskellのputStrLn関数のドキュメント](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:putStrLn)
- [HaskellのreadLn関数のドキュメント](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:readLn)
- [HaskellのDebug.Traceモジュールのドキュメント](https://hackage.haskell.org/package/base-4.14.0.0/docs/Debug-Trace.html)