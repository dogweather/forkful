---
title:                "ロギング"
aliases:
- ja/haskell/logging.md
date:                  2024-01-26T01:07:37.660826-07:00
model:                 gpt-4-1106-preview
simple_title:         "ロギング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/logging.md"
---

{{< edit_this_page >}}

## はじめに：なぜログを取るのか？
プログラミングにおけるログとは、アプリケーションがある瞬間に何をしているのかを追跡するために、イベントやメッセージを記録しておくことを意味しています。これは、パンくずの道のようなものです。プログラマーは、問題のデバッグ、システムパフォーマンスのモニタリング、セキュリティやコンプライアンス理由での行動監査を行うためにこれを実施します。

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
