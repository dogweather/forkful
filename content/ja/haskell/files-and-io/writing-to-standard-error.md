---
title:                "標準エラーへの書き込み"
aliases: - /ja/haskell/writing-to-standard-error.md
date:                  2024-02-03T19:33:32.541998-07:00
model:                 gpt-4-0125-preview
simple_title:         "標準エラーへの書き込み"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
Haskellで標準エラー（stderr）に書き込むことは、プログラムが通常の結果とエラーメッセージを区別できるようにすることを可能にします。これは、問題のシグナリングとデバッグに不可欠であり、プログラムの主要なデータや結果をしばしば持つ標準出力（stdout）をごちゃごちゃにしないようにします。

## 方法:
Haskellでstderrに書き込むことは、基本ライブラリの`System.IO`モジュールを使って簡単に行えます。以下に基本例を示します：

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "これはエラーメッセージです。"
```

このプログラムのstderrへの出力は以下の通りです：

```
これはエラーメッセージです。
```

もし、より複雑なアプリケーションで作業している場合、またはログ（エラーを含む）に対してより良い制御が必要な場合は、サードパーティのライブラリを選択するかもしれません。一つの人気な選択肢は、Haskellプログラミングの`mtl`スタイルと統合する`monad-logger`です。こちらは`monad-logger`を使用した小さなスニペットです：

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "これはmonad-loggerを使ったエラーメッセージです。"
```

実行時に、`monad-logger`バージョンも同様にエラーメッセージを出力しますが、設定に応じて、タイムスタンプやログレベルなどのより多くのコンテキストが装備されています：

```
[Error] これはmonad-loggerを使ったエラーメッセージです。
```

両方の方法はstderrに書き込む目的を果たしますが、選択は大きくアプリケーションの複雑さとニーズに依存します。
