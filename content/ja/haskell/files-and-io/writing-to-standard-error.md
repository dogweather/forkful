---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:32.541998-07:00
description: "\u65B9\u6CD5: Haskell\u3067stderr\u306B\u66F8\u304D\u8FBC\u3080\u3053\
  \u3068\u306F\u3001\u57FA\u672C\u30E9\u30A4\u30D6\u30E9\u30EA\u306E`System.IO`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u3063\u3066\u7C21\u5358\u306B\u884C\u3048\u307E\
  \u3059\u3002\u4EE5\u4E0B\u306B\u57FA\u672C\u4F8B\u3092\u793A\u3057\u307E\u3059\uFF1A\
  ."
lastmod: '2024-03-13T22:44:42.208522-06:00'
model: gpt-4-0125-preview
summary: "Haskell\u3067stderr\u306B\u66F8\u304D\u8FBC\u3080\u3053\u3068\u306F\u3001\
  \u57FA\u672C\u30E9\u30A4\u30D6\u30E9\u30EA\u306E`System.IO`\u30E2\u30B8\u30E5\u30FC\
  \u30EB\u3092\u4F7F\u3063\u3066\u7C21\u5358\u306B\u884C\u3048\u307E\u3059\u3002\u4EE5\
  \u4E0B\u306B\u57FA\u672C\u4F8B\u3092\u793A\u3057\u307E\u3059\uFF1A."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

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
