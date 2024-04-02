---
date: 2024-01-26 03:50:42.495692-07:00
description: "GHCi\u3067\u6563\u6B69\u3057\u3066\u307F\u307E\u3057\u3087\u3046\u3002\
  \u3053\u308C\u306FHaskell\u306E\u5BFE\u8A71\u578B\u74B0\u5883\u3067\u3001\u57FA\u672C\
  \u7684\u306A\u30C7\u30D0\u30C3\u30AC\u30FC\u3068\u3057\u3066\u6A5F\u80FD\u3059\u308B\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3042\u306A\u305F\u306FHaskell\u306E\
  \u30B3\u30FC\u30C9\u3067\u305D\u308C\u3092\u8D77\u52D5\u3057\u3001\u63A2\u7D22\u3092\
  \u59CB\u3081\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u4F8B\u3067\u3059\uFF1A ```Haskell\
  \ main :: IO () main = do putStrLn \"Hey, what's\u2026"
lastmod: '2024-03-13T22:44:42.190820-06:00'
model: gpt-4-0125-preview
summary: "GHCi\u3067\u6563\u6B69\u3057\u3066\u307F\u307E\u3057\u3087\u3046\u3002\u3053\
  \u308C\u306FHaskell\u306E\u5BFE\u8A71\u578B\u74B0\u5883\u3067\u3001\u57FA\u672C\u7684\
  \u306A\u30C7\u30D0\u30C3\u30AC\u30FC\u3068\u3057\u3066\u6A5F\u80FD\u3059\u308B\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3042\u306A\u305F\u306FHaskell\u306E\u30B3\
  \u30FC\u30C9\u3067\u305D\u308C\u3092\u8D77\u52D5\u3057\u3001\u63A2\u7D22\u3092\u59CB\
  \u3081\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u4F8B\u3067\u3059\uFF1A ```Haskell main\
  \ :: IO () main = do putStrLn \"Hey, what's\u2026"
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
weight: 35
---

## 方法：
GHCiで散歩してみましょう。これはHaskellの対話型環境で、基本的なデバッガーとして機能することができます。あなたはHaskellのコードでそれを起動し、探索を始めます。以下は例です：

```Haskell
main :: IO ()
main = do
    putStrLn "Hey, what's your name?"
    name <- getLine
    putStrLn $ "Hello, " ++ name ++ "! Let's debug."
    let result = buggyFunction 5
    print result

buggyFunction :: Int -> Int
buggyFunction n = n * 2 -- ここにバグがあると想像してください
```

GHCiでデバッグを開始するには：

```bash
$ ghci YourHaskellFile.hs
```

`buggyFunction`でブレークポイントを設定します：

```Haskell
Prelude> :break buggyFunction
```

プログラムを実行します：

```Haskell
Prelude> :main
Hey, what's your name?
```

プログラムは`buggyFunction`で一時停止します。これで変数を調査したり、コードをステップ実行したり、式を評価したりできます。

## 深堀り：
歴史的に、Haskellは純粋関数と強い型付けによる評価が高いため、デバッグツールはそれほど重要ではないと考えられていました。現実は異なります。複雑なプログラムは常に良いデバッグツールから恩恵を受けます。GHCiは基本的なデバッグコマンドを提供しています。しかし、より視覚的な体験や大規模なアプリケーションのために、Haskellの拡張機能を備えたVisual Studio CodeやIntelliJのHaskellプラグインなど、統合デバッガーを備えたIDEを探求することもできます。

デバッガの代わりになる方法としては、「printfデバッグ」として知られるprint文の使用や、誤った状態を表現できないようにするHaskellの強力な型システムを活用することがあります。しかし、時にはコードをステップ実行することに勝るものはありません。

実装の詳細として、Haskellのデバッガはランタイムシステムで動作します。ブレークポイントの処理、ステップ実行、変数の検査が可能です。しかし、Haskellは遅延評価されるため、少し直感的ではないことがあります。Haskellプログラムのデバッグは、式がいつ、どのように評価されるかを注視することを意味することがよくあります。

## 参照：
- [GHCユーザーズガイド - デバッガ](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [IntelliJ Haskellプラグイン](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)
