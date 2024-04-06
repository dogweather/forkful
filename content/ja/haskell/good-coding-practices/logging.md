---
date: 2024-01-26 01:07:37.660826-07:00
description: "\u65B9\u6CD5\uFF1A Haskell\u3067\u306E\u30ED\u30B0\u5B9F\u88C5\u306F\
  \u3001`monad-logger` \u3084 `hslogger` \u306E\u3088\u3046\u306A\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u7528\u3044\u3066\u884C\u3046\u3053\u3068\u304C\u3067\u304D\u307E\
  \u3059\u3002`monad-logger` \u3092\u4F7F\u3063\u305F\u7C21\u5358\u306A\u4F8B\u3092\
  \u4EE5\u4E0B\u306B\u793A\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.058051-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

## 方法：
Haskellでのログ実装は、`monad-logger` や `hslogger` のようなライブラリを用いて行うことができます。`monad-logger` を使った簡単な例を以下に示します：

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "アプリケーションを開始します..."
    liftIO $ putStrLn "重要な作業を実行中..."
    logErrorN "おっと！何か問題が発生しました。"

main :: IO ()
main = runStdoutLoggingT logExample

{- サンプル出力
[Info] アプリケーションを開始します...
重要な作業を実行中...
[Error] おっと！何か問題が発生しました。
-}
```

このシンプルな例では、ランタイムで何が起こっているのかを知るためにコードの随所にログ記述文を散りばめる方法を示しています。`logInfoN` と `logErrorN` はそれぞれ情報とエラーメッセージをログするために使われます。

## 深堀り：
ログは単純なprint文から洗練されたログフレームワークまで、長い道のりを歩んできました。歴史的には、ログとはコンソールやファイルへのテキスト出力に過ぎませんでしたが、現在では様々なツールによって解析される構造化されたデータを含むようになっています。

Haskellでのログは、ログアクションを明示的に渡す純粋関数型スタイルで行うことができますしまた不純物としてのモナディックコンテキストを使ってログを計算値を通じて暗黙的に扱うこともできます。

たとえば、`hslogger` ライブラリは、`monad-logger` と比較してより伝統的かつ可変性があります。`monad-logger` はモナドスタックとの統合を提供し、出力のフォーマットや制御においてより柔軟性があります。どちらのライブラリもログレベルの設定を可能にしており、重要度に基づいてログメッセージをフィルタリングするのに役立ちます。ログレベルには、デバッグ、情報、通知、警告、エラー、クリティカル、アラート、エマージェンシーがあります。

Haskellのログへのアプローチは、型の安全性と純粋性に重点を置いていることが多く、ログ処理が失敗しても、Haskellの堅牢なエラー処理能力のためにメインアプリケーションがクラッシュすることがないようにすることができます。

## 関連項目：
- [`monad-logger` のHackageにおけるドキュメント](https://hackage.haskell.org/package/monad-logger)
- [`hslogger` パッケージのHackage](https://hackage.haskell.org/package/hslogger)
- [実世界のHaskell、第19章、エラー処理について](http://book.realworldhaskell.org/read/error-handling.html)
- [Haskellのロギングファサード（log-base）](https://hackage.haskell.org/package/log-base)
